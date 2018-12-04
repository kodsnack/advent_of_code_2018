#!/usr/bin/env python3
import numpy as np

def addAtPos(mat1, mat2, xypos):
    """
    Add two matrices of different sizes in place, offset by xy coordinates
    Usage:
      - mat1: base matrix
      - mat2: add this matrix to mat1
      - xypos: tuple (x,y) containing coordinates
    """
    x, y = xypos
    mat2_ysize, mat2_xsize = mat2.shape
    xmax, ymax = (x + mat2_xsize), (y + mat2_ysize)
    mat1_ysize, mat1_xsize = mat1.shape
    if xmax > mat1_xsize or ymax > mat1_ysize:
        if xmax < mat1_xsize:
            mat0_xsize = mat1_xsize
        else:
            mat0_xsize = xmax
        if ymax < mat1_ysize:
            mat0_ysize = mat1_ysize
        else:
            mat0_ysize = ymax
        mat0 = np.zeros((mat0_ysize, mat0_xsize), dtype=int)
        mat1 = addAtPos(mat0, mat1, (0, 0))
        mat1_ysize, mat1_xsize = mat1.shape 
    mat1[y:ymax, x:xmax] += mat2
    return mat1

if __name__ == "__main__":
    with open("input.txt") as input_file:
        input_list = [line.strip() for line in input_file]
    
    # Part 1
    fabric = np.zeros((1, 1), dtype=int)
    for claim in input_list:
        cid, at, coord, size = claim.split(' ')
        cid = cid.strip('#')
        coord = coord.strip(':')
        xmin, ymin = coord.split(',')
        xsize, ysize = size.split('x')
        claim_array = np.ones((int(ysize), int(xsize)), dtype=int)
        # print("---------")
        # print("CID: {}".format(cid))
        # print("---------")
        # print(fabric)
        # print("+")
        # print(claim_array)
        # print("@")
        # print(xmin, ymin)
        fabric = addAtPos(fabric, claim_array, (int(xmin), int(ymin)))
        # print("=")
        # print(fabric)
    
    result = np.sum(fabric > 1)
    print("Part 1 answer: {}".format(result))
    
    # Part 2
    notFound = True
    while notFound:
        for claim in input_list:
            cid, at, coord, size = claim.split(' ')
            cid = cid.strip('#')
            coord = coord.strip(':')
            xmin, ymin = coord.split(',')
            xsize, ysize = size.split('x')
            xmin = int(xmin)
            ymin = int(ymin)
            xsize = int(xsize)
            ysize = int(ysize)
            claim_area = fabric[ymin:(ymin + ysize), xmin:(xmin + xsize)]
            if np.sum(claim_area > 1) == 0:
                notFound = False
                result = cid 
    print("Part 2 answer: Claim ID = {}".format(result))
