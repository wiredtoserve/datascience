'''
author: mehulnagpurkar

Edge detection is a technique in computer graphics that attempts to find the boundaries of objects in digital images.
The code will take a greyscale image as input and produce a black and white edge image as output.


Theory:

The Sobel filter approximates the magnitude of the local intensity gradient at each pixel coordinate.
Edges are identified when the gradient goes above a given threshold parameter.
The filter employs two 3 × 3 kernels to calculate the gradient at each image coordinate;
one approximates the gradient across rows,
the other across columns.
The row and column gradients are then combined to approximate the maximum magnitude of the intensity gradient.
This computation is applied at each coordinate in the input image in a process known as a convolution.
The magnitude of the gradient can be understood as a score of edge strength.
The larger the gradient the more confident we are of there being an edge at the corresponding coordinate.
The final output image is produced by applying a threshold to the gradient.
If the gradient is above the threshold then the corresponding output pixel is white, otherwise it is black.



'''

from math import *
from PIL import Image
from copy import deepcopy


def read_image(filename):
    """read_image(filename) -> list of lists of pixel intensities

    Output image is rectangular, grey-scale, 8 bits per pixel,
    in row major coordinates.
    """
    image = Image.open(filename)
    # Convert image to 8 bit per pixel grey-scale
    # if it is not already in that format.
    if image.mode != 'L':
        image = image.convert('L')
    assert (image.mode == 'L')
    pixels = list(image.getdata())
    # Convert the flat representation into a list of rows.
    width, height = image.size
    rows_cols = []
    for row in range(height):
        this_row = []
        row_offset = row * width
        for col in range(width):
            this_row.append(pixels[row_offset + col])
        rows_cols.append(this_row)
    return rows_cols


def write_image(image, filename):
    """write_image(image, filename) -> None

    Writes image data file to filename.

    Input image must be rectangular, grey-scale, 8 bits per pixel,
    in row major coordinates.
    """
    flat_pixels = []
    for row in image:
        flat_pixels += row
    out_image = Image.new('L', (get_width(image), get_height(image)))
    out_image.putdata(flat_pixels)
    out_image.save(filename)


def get_width(image):
    """get_width(image) -> integer width of the image (number of columns).

    Input image must be rectangular list of lists. The width is
    taken to be the length of the first row of pixels. If the image is
    empty, then the width is defined to be 0.
    """
    if len(image) == 0:
        return 0
    else:
        return len(image[0])


def get_height(image):
    """get_height(image) -> integer height of the image (number of rows).

    Input image must be rectangular list of lists. The height is
    taken to be the number of rows.
    """
    return len(image)


'''
We will address this problem by assuming that the pixels on the very boundary of the image are duplicated in imaginary 
rows and columns just outside the image. 
This can be achieved by clamping all row and column coordinates referred to by the kernels to ensure that they always 
stay within the image bounds. 
Any coordinate which goes outside the image bounds can be mapped to its nearest in-bounds neighbour.
'''


def clamp(val, lower_bound, upper_bound):
    return max(min(val, upper_bound), lower_bound)


def get_pixel(image, row, col):
    max_row = get_height(image) - 1
    max_col = get_width(image) - 1
    new_row = clamp(row, 0, max_row)
    new_col = clamp(col, 0, max_col)
    return image[new_row][new_col]


def gradient_row(image, row, col):
    """
    Implement a Python function gradient_row(image, row, col) that computes an approximation to
    the row gradient at the coordinate (row, col) in image.

    :param image: user wants to process
    :param row: row number
    :param col: column number
    :return: partial differential of the row gradient
    """
    count = 0
    gradient = 0
    matrix = [-1, 0, 1]
    kernel = ((1, 2, 1), (0, 0, 0), (-1, -2, -1))

    # Maximum length and width of the image
    length = get_height(image) - 1
    width = get_width(image) - 1

    for n in matrix:
        for m in matrix:

            if (0 < row < length) and (0 < col < width):
                gradient += image[row + n][col + m] * kernel[count][m + 1]
            else:
                gradient += get_pixel(image, row + n, col + m) * kernel[count][m + 1]

        count += 1

    return gradient


def gradient_column(image, row, col):
    """
        Implement a Python function gradient_column(image, row, col) that computes an approximation to
        the column gradient at the coordinate (row, col) in image.

        :param image: user wants to process
        :param row: row number
        :param col: column number
        :return: partial differential of the column gradient
    """
    count = 0
    gradient = 0
    col_matrix = [1, 0, -1]
    kernel = ((1, 2, 1), (0, 0, 0), (-1, -2, -1))

    # Maximum length and width of the image
    length = get_height(image) - 1
    width = get_width(image) - 1

    for n in col_matrix:
        for m in col_matrix:

            if (0 < row < length) and (0 < col < width):
                gradient += image[row + m][col + n] * kernel[count][m + 1]
            else:
                gradient += get_pixel(image, row + m, col + n) * kernel[count][m + 1]

        count += 1

    return gradient


def gradient_magnitude(image, row, col):
    """
    Implement a Python function gradient_magnitude(image, row, col) that computes an approximation
     to the gradient magnitude at the coordinate (row, col) in image.

    :return: gradient magnitude approximation
    """
    return sqrt((gradient_row(image, row, col)**2)+(gradient_column(image, row, col)**2))


def gradient_threshold(image, row, col, threshold):
    """
    Implement a Python function gradient_threshold(image, row, col, threshold) that computes the edge pixel intensity
    at the coordinate (row, col) in image, for the given threshold value.

    :param threshold: threshold value for filtering

    :return: 255 (True) if the gradient is above the threshold, 0 (False) otherwise
    """
    return 255 if gradient_magnitude(image, row, col) > threshold else 0


def convolute(image, threshold):
    """
    Implement a Python function convolute(image, threshold) that computes an edge image
     for the input image and threshold. The output edge image must have the same number of rows and columns
     as the input image. All intensity values in the output edge image must be either 0 or 255.
    Your implementation of convolute must not modify the input image,
    instead it must construct a completely new image as output.


    :return: new convoluted image as output
    """

    # Maximum length and width of the image
    length = get_height(image)
    width = get_width(image)

    convolute_image = deepcopy(image)

    for row in range(length):
        for col in range(width):
            convolute_image[row][col] = gradient_threshold(image, row, col, threshold)

    return convolute_image


def edge_detect(in_filename, out_filename, threshold=200):
    """
    The function edge_detect an be used to test your implementation of convolute on real image files

    :param in_filename: input picture (png)
    :param out_filename: input picture (png)
    :param threshold: filter value (default = 200)

    :return: writes the new filtered image
    """
    in_image = read_image(in_filename)
    out_image = convolute(in_image, threshold)
    write_image(out_image,out_filename+'_'+str(threshold)+'.png')


def main():
    # Sobel Filter
    matrix = [-1, 0, 1]
    kernel = ((1, 2, 1), (0, 0, 0), (-1, -2, -1))
    col_matrix = [1, 0, -1]

    # Running the edge detection code
    edge_detect('batman.png', 'batman_edge', 50)

main()
