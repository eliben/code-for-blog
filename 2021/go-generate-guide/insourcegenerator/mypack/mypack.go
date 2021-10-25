package mypack

//go:generate go run gen.go arg1 arg2

func PackFunc() string {
	return "insourcegenerator/mypack.PackFunc"
}
