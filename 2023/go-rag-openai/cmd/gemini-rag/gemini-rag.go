// RAG using Google's Gemini model instead of OpenAI.
//
// Export your API_KEY (see https://ai.google.dev/) first.
// The same `chunker` tool from the parent dir may be reused to create the
// initial SQLite DB with text chunks.
//
// Run with --calculate to calculate embeddings for all entries in the DB
// and update the DB. This consumes quite a bit of API to calculate embeddings.
//
// Run with --answer to augment the question with the most relevant information
// found in the DB.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"bytes"
	"context"
	"database/sql"
	"encoding/binary"
	"flag"
	"fmt"
	"log"
	"os"
	"slices"

	"github.com/chewxy/math32"
	"github.com/google/generative-ai-go/genai"
	_ "github.com/mattn/go-sqlite3"
	"google.golang.org/api/option"
)

const generativeModel = "gemini-pro"
const embeddingModel = "embedding-001"
const theQuestion = "what does GOTOOLCHAIN mean in go?"

func main() {
	dbPath := flag.String("db", "chunks.db", "path to DB with chunks")
	doCalculate := flag.Bool("calculate", false, "calculate embeddings and update DB")
	doAnswer := flag.Bool("answer", false, "answer question (DB must have embeddings already)")
	flag.Parse()

	if *doAnswer {
		answerQuestion(*dbPath)
	} else if *doCalculate {
		calculateEmbeddings(*dbPath)
	}
}

func answerQuestion(dbPath string) {
	client, err := genai.NewClient(context.Background(), option.WithAPIKey(os.Getenv("API_KEY")))
	checkErr(err)

	// Connect to the SQLite database
	db, err := sql.Open("sqlite3", dbPath)
	checkErr(err)
	defer db.Close()

	// SQL query to extract chunks' content along with embeddings.
	stmt, err := db.Prepare(`
	SELECT chunks.path, chunks.content, embeddings.embedding
	FROM chunks
	INNER JOIN embeddings
	ON chunks.id = embeddings.id
	`)
	checkErr(err)
	defer stmt.Close()

	rows, err := stmt.Query()
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	type scoreRecord struct {
		Path    string
		Score   float32
		Content string
	}
	var scores []scoreRecord

	// Iterate through the rows, scoring each chunk with cosine similarity to
	// the question's embedding.
	qEmb := getEmbedding(theQuestion)
	for rows.Next() {
		var (
			path      string
			content   string
			embedding []byte
		)

		err = rows.Scan(&path, &content, &embedding)
		if err != nil {
			log.Fatal(err)
		}

		fmt.Printf("path: %s, content: %d, embedding: %d\n", path, len(content), len(embedding))

		contentEmb := decodeEmbedding(embedding)
		score := cosineSimilarity(qEmb, contentEmb)
		scores = append(scores, scoreRecord{path, score, content})
		fmt.Println(path, score)
	}
	if err = rows.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Println(len(scores))
	slices.SortFunc(scores, func(a, b scoreRecord) int {
		// The scores are in the range [0, 1], so scale them to get non-zero
		// integers for comparison.
		return int(100.0 * (a.Score - b.Score))
	})

	// Take the 3 best-scoring chunks as context and paste them together into
	// contextInfo.
	var contextInfo string
	for i := len(scores) - 1; i > len(scores)-4; i-- {
		contextInfo = contextInfo + "\n" + scores[i].Content
	}

	// Build the prompt and execute the LLM API.
	query := fmt.Sprintf(`Use the below information to answer the subsequent question.
	Information:
	%v

	Question: %v`, contextInfo, theQuestion)
	fmt.Println("Query:\n", query)

	model := client.GenerativeModel(generativeModel)
	resp, err := model.GenerateContent(context.Background(), genai.Text(query))
	checkErr(err)

	for _, cand := range resp.Candidates {
		for _, part := range cand.Content.Parts {
			fmt.Println(part)
		}
	}
	fmt.Println("---")
}

func calculateEmbeddings(dbPath string) {
	db, err := sql.Open("sqlite3", dbPath)
	checkErr(err)
	defer db.Close()

	log.Println("Creating embeddings table if needed")
	_, err = db.Exec(`
	CREATE TABLE IF NOT EXISTS embeddings (
	id INTEGER PRIMARY KEY,
	embedding BLOB
	)`)
	checkErr(err)

	log.Println("Clearing embeddings table")
	_, err = db.Exec(`DELETE FROM embeddings`)
	checkErr(err)

	rows, err := db.Query("SELECT * FROM chunks")
	checkErr(err)
	defer rows.Close()

	// Step 1: calculate embeddings for all chunks in the DB, storing them in
	// embs.
	type embData struct {
		id   int
		data []byte
	}
	var embs []embData

	for rows.Next() {
		var (
			id      int
			path    string
			nchunk  int
			content string
		)
		err = rows.Scan(&id, &path, &nchunk, &content)
		checkErr(err)

		fmt.Printf("id: %d, path: %s, nchunk: %d, content: %d\n", id, path, nchunk, len(content))
		if len(content) > 0 {
			emb := encodeEmbedding(getEmbedding(content))
			embs = append(embs, embData{id, emb})
			fmt.Println(id, len(emb))
		}
	}

	if err = rows.Err(); err != nil {
		log.Fatal(err)
	}
	rows.Close()

	// Step 2: insert all embedding data into the embeddings table.
	for _, emb := range embs {
		fmt.Println("Inserting into embeddings, id", emb.id)
		_, err = db.Exec("INSERT INTO embeddings VALUES (?, ?)", emb.id, emb.data)
		checkErr(err)
	}
}

// getEmbedding invokes the Gemini embedding API to calculate the embedding
// for the given string. It returns the embedding.
func getEmbedding(data string) []float32 {
	client, err := genai.NewClient(context.Background(), option.WithAPIKey(os.Getenv("API_KEY")))
	checkErr(err)
	defer client.Close()

	em := client.EmbeddingModel(embeddingModel)
	res, err := em.EmbedContent(context.Background(), genai.Text(data))
	checkErr(err)
	return res.Embedding.Values
}

// encodeEmbedding encodes an embedding into a byte buffer, e.g. for DB
// storage as a blob.
func encodeEmbedding(emb []float32) []byte {
	buf := new(bytes.Buffer)
	for _, f := range emb {
		err := binary.Write(buf, binary.LittleEndian, f)
		checkErr(err)
	}
	return buf.Bytes()
}

// decodeEmbedding decodes an embedding back from a byte buffer.
func decodeEmbedding(b []byte) []float32 {
	var numbers []float32
	buf := bytes.NewReader(b)

	// Calculate how many float32 values are in the slice
	count := buf.Len() / 4

	for i := 0; i < count; i++ {
		var num float32
		err := binary.Read(buf, binary.LittleEndian, &num)
		checkErr(err)
		numbers = append(numbers, num)
	}
	return numbers
}

// cosineSimilarity calculates cosine similarity (magnitude-adjusted dot
// product) between two vectors that must be of the same size.
func cosineSimilarity(a, b []float32) float32 {
	if len(a) != len(b) {
		panic("different lengths")
	}

	var aMag, bMag, dotProduct float32
	for i := 0; i < len(a); i++ {
		aMag += a[i] * a[i]
		bMag += b[i] * b[i]
		dotProduct += a[i] * b[i]
	}
	return dotProduct / (math32.Sqrt(aMag) * math32.Sqrt(bMag))
}

func checkErr(err error) {
	if err != nil {
		panic(err)
	}
}
