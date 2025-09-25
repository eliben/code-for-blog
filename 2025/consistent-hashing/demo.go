package main

import (
	"fmt"
	"html/template"
	"math/rand/v2"
	"os"
	"slices"
	"strings"
)

func demo1() {
	items := []string{"hello", "consistent", "marmot"}

	var n uint64 = 32
	for _, item := range items {
		fmt.Printf("%-11v (n=%v): %v\n", item, n, hashItem(item, n))
	}

	fmt.Println("")
	n = 33
	for _, item := range items {
		fmt.Printf("%-11v (n=%v): %v\n", item, n, hashItem(item, n))
	}
}

func dumpDistribution() {
	s1, s2 := rand.Uint64(), rand.Uint64()
	rnd := rand.New(rand.NewPCG(s1, s2))
	ch := NewConsistentHasher(1024 * 1024)

	var nodes []string
	for i := range 16 {
		n := fmt.Sprintf("n%03d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			panic(err)
		}
	}

	cnt := make(map[string]int)
	for range 100000 {
		str := generateRandomString(rnd, 16)
		nn := ch.FindNodeFor(str)
		cnt[nn] += 1
	}

	for n, c := range cnt {
		fmt.Println(n, c)
	}
}

func dumpNodeVarianceStats() {
	ch := NewConsistentHasher(10000000)

	var nodes []string
	for i := range 20 {
		n := fmt.Sprintf("n-%02d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			panic(err)
		}
	}

	var sum float32
	var mn float32 = 1.0
	var mx float32 = 0.0
	for i := range ch.slots {
		var gap uint64
		if i == 0 {
			gap = ch.ringSize + ch.slots[0] - ch.slots[len(ch.slots)-1]
		} else {
			gap = ch.slots[i] - ch.slots[i-1]
		}

		gapf := float32(gap) / float32(ch.ringSize)
		sum += gapf
		mn = min(gapf, mn)
		mx = max(gapf, mx)
	}
	avg := 360.0 * sum / float32(len(ch.slots))
	mn = 360.0 * mn
	mx = 360.0 * mx

	fmt.Printf("In degrees, avg=%.2f, min=%.2f, max=%.2f\n", avg, mn, mx)
}

func dumpNodeVarianceStatsV() {
	ch := NewConsistentHasherV(10000000)

	var nodes []string
	for i := range 20 {
		n := fmt.Sprintf("n%02d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			panic(err)
		}
	}

	// For each node, holds a list of the gaps for its vnodes
	gaps := make(map[string][]float32)
	for i := range ch.slots {
		var gap uint64
		if i == 0 {
			gap = ch.ringSize + ch.slots[0] - ch.slots[len(ch.slots)-1]
		} else {
			gap = ch.slots[i] - ch.slots[i-1]
		}

		gapf := float32(gap) / float32(ch.ringSize)
		node := nodeFromVnode(ch.nodes[i])
		gaps[node] = append(gaps[node], gapf)
	}

	var sum float32
	var mn float32 = 1.0
	var mx float32 = 0.0

	for _, gs := range gaps {
		var total float32
		for _, g := range gs {
			total += g
		}

		sum += total
		mn = min(mn, total)
		mx = max(mx, total)
	}
	avg := 360 * sum / float32(len(gaps))
	mn = 360 * mn
	mx = 360 * mx

	fmt.Printf("In degrees, avg=%.2f, min=%.2f, max=%.2f\n", avg, mn, mx)
}

var tikzTemplate = template.Must(template.New("t").Parse(`
\documentclass[tikz]{standalone}
\usepackage{tikz}
\usetikzlibrary{arrows.meta}

\definecolor{Oc}{RGB}{0, 0, 0}

\begin{document}
	\begin{tikzpicture}[>=Stealth, line cap=round, line join=round, scale=2.8]
	\pgfextra{\path[use as bounding box] (-1.3,-1.3) rectangle (1.3,1.3);}
    \def\R{1.0}
	\draw[thick] (0,0) circle (\R);
	
	% Angles array
	\def\angles#1{\ifcase#1 {{.Angles}}\fi}
	
	\foreach \i in { 0,...,{{.RangeEnd}} }{
		 \pgfmathsetmacro{\ang}{\angles{\i}}
		% draw dot
		\fill[Oc] (\ang:\R) circle (1.1pt);
	}
	\end{tikzpicture}
\end{document}
`))

func dumpRandomNodesTikz() {
	var fullSize uint = 1024 * 1024
	var angles []float32

	for range 20 {
		n := rand.UintN(fullSize)
		fraction := float32(n) / float32(fullSize)
		angle := fraction * 360.0
		angles = append(angles, angle)
	}
	slices.Sort(angles)
	fmt.Println(angles)

	var deltas []float32
	var sum float32

	for i, ang := range angles {
		var delta float32
		if i == 0 {
			delta = 360.0 + ang - angles[len(angles)-1]
		} else {
			delta = ang - angles[i-1]
		}
		deltas = append(deltas, delta)
		sum += delta
	}

	avg := sum / float32(len(deltas))
	mn := slices.Min(deltas)
	mx := slices.Max(deltas)

	fmt.Printf("%% average=%f, min=%v, max=%v\n", avg, mn, mx)

	var sangles []string
	for _, ang := range angles {
		sangles = append(sangles, fmt.Sprintf("%d", int(ang)))
	}
	tikzTemplate.Execute(os.Stdout, map[string]string{
		"Angles":   strings.Join(sangles, `\or `),
		"RangeEnd": fmt.Sprintf("%d", len(sangles)-1),
	})
}

func dumpNodesCircleTikz() {
	ch := NewConsistentHasher(1024 * 1024)

	var nodes []string
	for i := range 16 {
		n := fmt.Sprintf("n%003d", i)
		nodes = append(nodes, n)
		if err := ch.AddNode(n); err != nil {
			panic(err)
		}
	}

	var angles []string

	// For each node index, compute the angle it represents. ringSize is 360
	// degrees.
	for _, s := range ch.slots {
		fraction := float32(s) / float32(ch.ringSize)
		angle := fraction * 360.0
		angles = append(angles, fmt.Sprintf("%d", int(angle)))
	}

	tikzTemplate.Execute(os.Stdout, map[string]string{
		"Angles":   strings.Join(angles, `\or `),
		"RangeEnd": fmt.Sprintf("%d", len(angles)-1),
	})
}

func main() {
	//demo1()
	//dumpDistribution()
	//dumpNodesCircleTikz()
	//dumpRandomNodesTikz()
	//dumpNodeVarianceStats()
	dumpNodeVarianceStatsV()
}
