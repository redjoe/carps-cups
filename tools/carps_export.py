#!/usr/bin/python3
import argparse
import pathlib
import logging
from carps_decode import (parse_blocks, parse_data_print_block, parse_header_image, 
                          DataType, BlockType, MyDataBlockType)


logging.getLogger()
logging.basicConfig(level=logging.DEBUG)


def save_img_file(data, path, filename="printfile", page=0):
    er_path = path / filename
    with open(f"{er_path}-p{page}.g4", "wb") as f:
        f.write(data)


def main(args: argparse.Namespace):
    filename = pathlib.PurePosixPath(args.infile.name).stem
    blocks_list = parse_blocks(args.infile.read())
    page = 1
    result = b""
    width = 0
    heigth = 0
    for header, data in blocks_list:
        if header.data_type == DataType.PRINT and header.block_type == BlockType.PRINT:
            type_block, data = parse_data_print_block(header, data)
            print(type_block)
            if type_block == MyDataBlockType.IMAGE:
                data, temp_w, temp_h = parse_header_image(data)
                if temp_w != -1 and temp_w != -1:
                    width = temp_w
                    heigth = temp_h
                result += data
            elif type_block == MyDataBlockType.PAGE_END:
                save_img_file(result, path=args.outpath, filename=filename, page=page)
                result = b""
                page += 1
    print(f'Export success pages {page-1}\nuse command for convert faxg4 to tiff')
    for page_num in range(1, page):
        print(f'fax2tiff -4 -X {width} {filename}-p{page_num}.g4 -o {filename}-p{page_num}.tiff')
        
                

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("infile", type=argparse.FileType("rb") )
    parser.add_argument("outpath", nargs='?', type=pathlib.Path, default=pathlib.Path.cwd())
    args = parser.parse_args()

    main(args)