// Command chunker processes a directory of MarkDown files, splitting all
// content to chunks of roughly the same size, and stores each chunk along
// with identifying information into a SQLite database.
package main

import (
	"bufio"
	"bytes"
	"database/sql"
	"flag"
	"fmt"
	"io/fs"
	"log"
	"os"
	"path/filepath"

	_ "github.com/mattn/go-sqlite3"
	"github.com/pkoukk/tiktoken-go"
)

const tokenEncoding = "cl100k_base"
const tableName = "chunks"
const chunkSize = 1000

const tableSql = `CREATE TABLE IF NOT EXISTS %v (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  path TEXT,
  nchunk INTEGER,
  content TEXT
);`

func main() {
	rootDir := flag.String("rootdir", ".", "root directory to take MD files from")
	doClear := flag.Bool("clear", false, "clear DB table before inserting")
	outDb := flag.String("outdb", "chunks.db", "filename for sqlite DB for output")
	flag.Parse()

	db, err := sql.Open("sqlite3", *outDb)
	checkErr(err)

	_, err = db.Exec(fmt.Sprintf(tableSql, tableName))
	checkErr(err)

	if *doClear {
		log.Printf("Clearing DB table %v", tableName)
		_, err := db.Exec(fmt.Sprintf("delete from %s", tableName))
		checkErr(err)
	}

	insertStmt, err := db.Prepare("insert into chunks(path, nchunk, content) values(?, ?, ?)")
	checkErr(err)

	tokTotal := 0
	err = filepath.WalkDir(*rootDir, func(path string, d fs.DirEntry, err error) error {
		if filepath.Ext(path) == ".md" {
			log.Printf("Chunking %v", path)
			chunks := breakToChunks(path)

			for i, chunk := range chunks {
				fmt.Println(path, i, len(chunk))
				tokTotal += len(chunk)
				_, err := insertStmt.Exec(path, i, chunk)
				checkErr(err)
			}

		}
		return nil
	})
	fmt.Println("Total tokens:", tokTotal)
}

func checkErr(err error) {
	if err != nil {
		panic(err)
	}
}

// breakToChunks reads the file in `path` and breaks it into chunks of
// approximately chunkSize tokens each, returning the chunks.
func breakToChunks(path string) []string {
	f, err := os.Open(path)
	checkErr(err)

	tke, err := tiktoken.GetEncoding(tokenEncoding)
	checkErr(err)

	chunks := []string{""}

	scanner := bufio.NewScanner(f)
	scanner.Split(splitByParagraph)

	for scanner.Scan() {
		chunks[len(chunks)-1] = chunks[len(chunks)-1] + scanner.Text() + "\n"
		toks := tke.Encode(chunks[len(chunks)-1], nil, nil)
		if len(toks) > chunkSize {
			chunks = append(chunks, "")
		}
	}

	// If we added a new empty chunk but there weren't any paragraphs to add to
	// it, make sure to remove it.
	if len(chunks[len(chunks)-1]) == 0 {
		chunks = chunks[:len(chunks)-1]
	}

	return chunks
}

// splitByParagraph is a custom split function for bufio.Scanner to split by
// paragraphs (text pieces separated by two newlines).
func splitByParagraph(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if i := bytes.Index(data, []byte("\n\n")); i >= 0 {
		return i + 2, bytes.TrimSpace(data[:i]), nil
	}

	if atEOF && len(data) != 0 {
		return len(data), bytes.TrimSpace(data), nil
	}

	return 0, nil, nil
}
