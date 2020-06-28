# bmp

A library for loading and storing bmp files.

## Usage

The library provides a single record type `bmp`.

	make-bmp
	bmp?
	bmp-height
	bmp-width
	bmp-bytevector

The record contains the values for the width and height of an image. It also contains a bytevector of pixel data stored in BRGA888 format.

	make-bmp-from-path
	make-bmp-from-port

These procedures take a file system path or a port to a BMP file and will return a `bmp` record constructed from that file.

    save-bmp-to-path
    save-bmp-to-port

These procedures take a `bmp` record and, will store a BMP file to a file system path or port.
