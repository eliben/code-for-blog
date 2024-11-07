// Downloaded "CIFAR-10 binary version (suitable for C programs)" from
// https://www.cs.toronto.edu/~kriz/cifar.html
//
// The format is explained in Dataset Layout --> Binary version on that page.
package main

import (
	"fmt"
	"image"
	"image/color"
	"image/png"
	"io"
	"log"
	"os"
)

// CifarImage represents a single image in a CIFAR-10 dataset. It contains
// the image itself, the label ID for this image (a number 0-9) and the
// class this label corresponds to (based on the lookup table in labelClasses).
type CifarImage struct {
	img        image.Image
	labelID    uint8
	labelClass string
}

var labelClasses = []string{
	"airplane",
	"automobile",
	"bird",
	"cat",
	"deer",
	"dog",
	"frog",
	"horse",
	"ship",
	"truck",
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: load-cifar-images <cifar-10-binary-file>")
		os.Exit(1)
	}

	f, err := os.Open(os.Args[1])
	if err != nil {
		panic(err)
	}
	defer f.Close()

	ci := loadCifarImages(f)
	fmt.Println("total images:", len(ci))

	for i := range 20 {
		filename := fmt.Sprintf("image%02d.png", i)
		writeImagePNG(ci[i].img, filename)
		fmt.Printf("%02d: %s --> %s\n", i, filename, ci[i].labelClass)
	}
}

func writeImagePNG(img image.Image, filename string) {
	f, err := os.Create(filename)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	if err := png.Encode(f, img); err != nil {
		panic(err)
	}
}

func loadCifarImages(r io.Reader) []CifarImage {
	// Each image in the CIFAR binary encoding is 1 byte for label and
	// 32*32*3=3072 bytes for the pixel data (32x32, 3 color channels).
	dim := 32
	var results []CifarImage

	for {
		img := image.NewRGBA(image.Rect(0, 0, dim, dim))

		buf := make([]byte, 3073)
		_, err := io.ReadFull(r, buf)
		if err == io.EOF {
			return results
		} else if err != nil {
			log.Println("file read error:", err)
		}

		labelID := uint8(buf[0])
		buf = buf[1:]
		// The image is stored row-major, with the red first, then green, then
		// blue.
		for x := range dim {
			for y := range dim {
				idx := y*32 + x
				red := buf[idx]
				green := buf[1024+idx]
				blue := buf[2048+idx]
				img.Set(x, y, color.RGBA{red, green, blue, 0xff})
			}
		}

		results = append(results,
			CifarImage{
				img:        img,
				labelID:    labelID,
				labelClass: labelClasses[labelID],
			})
	}
}
