# KnightOS images format

**Note**: All details may change before the initial release.

Images, due to their potential size, can be compressed or uncompressed and using a palette or not.
These information are given as a header defined as follows :

<table>
    <th>Offset</th><th>Length (bytes)</th><th>Use</th>
    <tr><td>0x00</td><td>4</td><td>Magic string "KIMG"</td></tr>
    <tr><td>0x04</td><td>1</td><td>Format (see below)</td></tr>
    <tr><td>0x05</td><td>2</td><td>Width in pixels</td></tr>
    <tr><td>0x07</td><td>1</td><td>Height in pixels</td></tr>
    <tr><td>0x08</td><td>1</td><td>How many palette entries if necessary</td></tr>
    <tr><td>0x09</td><td>x</td><td>2-bytes palette entries if necessary</td></tr>
    <tr><td>0x..</td><td>x</td><td>Image data</td></tr>
</table>

The format byte is defined as follows :

<table>
    <th>Bit 7</th><th>6</th><th>5</th><th>4</th><th>3 - 2</th><th>1</th><th>0</th>
    <tr><td> </td><td> </td><td> </td><td> </td><td>Compression technique</td><td>Uses palette ?</td><td>Color image ?</td></tr>
</table>

The width must not be over 320 for a color image and not over 96 for a monochrome image, and the height must not be over 240 for a color image
and not over 64 for a monochrome image.

A monochrome image can not use a palette, but can be compressed. The compression technique used is defined by two bits as follows :

<table>
    <th>00</th><th>01</th><th>10</th><th>11</th>
    <tr><td>No compression</td><td>RLE</td><td>Reserved for future use</td><td>Reserved for future use</td></tr>
</table>

If the image is both compressed and paletted, it should be decompressed and then displayed using the palette that will pop out of the decompressed
data. If the image uses no palette, that means the image data should be located at offset 0x04. If the image is compressed, the entire header except
the format byte and the three size bytes should be compressed with it.

## Raw image data

This is a "normal" image ; uncompressed and using no palette, it's already ready for direct write to the LCD or a screen buffer.
They are the fastest image type to work with, but can rapidly take a lot of memory. For a color image, each pixel is encoded in
R5G6B5 format, thus taking two bytes. For a monochrome image, each byte encodes 8 pixels.

The header for a raw monochrome image would be as follows :

```
    .db "KIMG"
    .db 0b00000000 ; non-compressed and non-paletted monochrome image
    .dw 96 ; width of 96 pixels
    .db 64 ; height of 64 pixels
    .db 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff ; one row is 12 bytes since 12 * 8 = 96 pixels
    ; etc ...
```
And the header for a raw color image would be the same, except that the header would be `0b00000001` and each pixel would be two bytes long instead
of a word.

## Paletted images

Only color images can use palettes. Instead of using two bytes per pixel to indicate its color in R5G6B5 format, each pixel is one byte and is used
as an offset to a table of colors.

The header for a paletted color image would be as follows :

```
    .db "KIMG"
    .db 0b00000011 ; paletted color image
    .dw 16
    .db 16
    .db 4 ; 4 possible colors
    .dw 0x0000, 0x00f1, 0xe007, 0x1f00 ; the 4 colors
    .db 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 ; 16 one-byte pixels
    ; etc ...
```
