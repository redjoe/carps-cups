#!/usr/bin/python3
import sys
import struct
from enum import Enum, IntFlag, IntEnum
from dataclasses import dataclass
from typing import List, Tuple
import logging
import argparse


logger = logging.getLogger(__name__)


class DataType(Enum):
    CONTROL = b'\x00'
    PRINT = b'\x02'


class BlockType(IntFlag):
    DOC_INFO = 0x6b
    BEGIN1 = 0x14
    BEGIN2 = 0x17
    PARAMS = 0x18
    PRINT = 0x1a
    END2 = 0x19
    END1 = 0x16
    END = 0x13
    

class ParamFlag(IntEnum):
    DISABLED = 1
    ENABLED = 2


class PaperWeight(IntEnum):
    PLAIN_L	= 15
    PLAIN	= 20
    HEAVY	= 30
    HEAVY_H	= 35
    TRANSP	= 40
    ENVELOPE_C = 50 # ENV_C5, ENV_COM10
    ENVELOPE = 55  # ENV_DL ENV_MONAR


class PaperSize(IntEnum):
    A4	= 14
    A5	= 16
    B5	= 26
    LETTER	= 30
    LEGAL	= 32
    EXECUTIVE	= 40
    ENV_MONAR	= 60
    ENV_COM10	= 62
    ENV_DL	= 64
    ENV_C5	= 66
    CUSTOM	= 80


class MyDataBlockType(IntEnum):
    START = 1
    IMAGE = 2
    PAGE_END = 3
    DOC_END = 4
    
  
@dataclass
class CarpsHeader():
    magic1: bytes  # 0xCD
    magic2: bytes  # 0xCA
    magic3: bytes  # 0x10
    data_type: DataType  # 0x00 = control data, 0x02 = print data
    zero1: bytes  # 0x00
    block_type: BlockType  # 0x11, 0x12, 0x13, 0x14, 0x16, 0x17, 0x18, 0x19, 0x1a
    zero2: bytes  # 0x00
    one: bytes  # 0x01
    data_len: int  # number of following data bytes, big endian
    empty: bytes  # empty[10] zeros


def parse_carps_header(data: bytes) -> CarpsHeader:
    (
        magic1,
        magic2,
        magic3,
        data_type,
        zero1,
        block_type,
        zero2,
        one,
        data_len,
    ) = struct.unpack('>ccccccccH', data[:10])
    if magic1 != b'\xCD': logger.error(f"{magic1=} != 0xCD")
    if magic2 != b'\xCA': logger.error(f"{magic2=} != 0xCA")
    if magic3 != b'\x10': logger.error(f"{magic3=} != 0x10")
    if zero1 != b'\x00': logger.error(f"{zero1=} != 0x00")
    if zero2 != b'\x00': logger.error(f"{zero2=} != 0x00")
    if one != b'\x01': logger.error(f"{one=} != 0x01")
    assert magic1 == b'\xCD'
    assert magic2 == b'\xCA'
    assert magic3 == b'\x10'
    data_type = DataType(data_type)
    block_type = BlockType(ord(block_type))
    if block_type not in set(BlockType): logger.warn(f"NO in BlockType {block_type} {hex(block_type)}")
    return CarpsHeader(magic1, magic2, magic3, data_type, zero1, block_type, zero2, one, data_len, data[10:])


def parse_header_data_print_block(data: bytes) -> Tuple[int, int]:
    """
    param data: bytes line
    return: width, heigth
    """
    list_parts = data.split(b";")
    return int(list_parts[1].decode()), int(list_parts[2].decode())


def parse_first_data_print_block(data: bytes) -> Tuple[int, PaperWeight, PaperSize, int]:
    # 0 b''
    # 1 b'%@'
    # 2 b'P42;600;1J;ImgColor'      ??? and resolution
    # 3 b'\\'                       ???
    # 4 b'[11h'                     ???
    # 5 b'[?7;600 I'                ??? and resolution
    # 6 b"[20't"
    # 7 b'[14;;;;;;p'
    # 8 b'[?2h'                     ???
    # 9 b'[1v'
    # 10 b"[600;1;0;256;;0;0'c"     resolution and ???
    list_parts = data.split(b"\x1b")
    for ind, part in enumerate(list_parts):
        if ind == 2:
            dpi = int(part.split(b";")[1].decode())
            logger.debug(part)
            logger.info("dpi=%s", dpi)
        elif ind == 6:
            paper_weight = PaperWeight(int(part[1:3]))
            logger.info(paper_weight)
        elif ind == 7:
            paper_size, paper_height, paper_width, margin1, margin2, margin3, margin4 = part.split(b";")
            paper_size = PaperSize(int(paper_size[1:]))
            if paper_size == PaperSize.CUSTOM:
                logger.info(f"{paper_size} {paper_width=}, {paper_height=}, margin:{margin1}{margin2}{margin3}{margin4}")
            else:
                logger.info(paper_size)
        elif ind == 9:
            copies = int(part[1:].removesuffix(b"v"))
            logger.info(f"{copies=}")
        else:
            pass
            logger.debug("index=%s, %s", ind, part)
    return dpi, paper_weight, paper_size, copies


def parse_header_image(data: bytes, sep=b"16.P") -> Tuple[bytes, int, int]:
    separate = data.find(sep)
    if separate != -1:
        width, heigth = parse_header_data_print_block(data[:separate+2])
        logger.info(f'Page {width=} {heigth=}')
        return data[separate+len(sep):], width, heigth
    return data, -1, -1


def parse_data_print_block(header: CarpsHeader, data: bytes) -> Tuple[MyDataBlockType, bytes]:
    data = data[2:]
    if data == b'\x1bP0J\x1b\\':
        # End block
        logger.debug("%s %s", MyDataBlockType.DOC_END, header.data_type)
        return MyDataBlockType.DOC_END, data
    elif data == b'\x0c':
        # Separete page??
        logger.debug("%s %s", MyDataBlockType.PAGE_END, header.data_type)
        return MyDataBlockType.PAGE_END, data
    elif data[1] == ord('%'):
        # First block
        logger.debug("%s %s", MyDataBlockType.START, header.data_type)
        parse_first_data_print_block(data)
        return MyDataBlockType.START, data
    logger.debug("%s %s", MyDataBlockType.IMAGE, header.data_type)
    return MyDataBlockType.IMAGE, data
    

def parse_blocks(data: bytes) -> List[Tuple[CarpsHeader, bytes]]:
    blocks = []
    while data:
        carps_header = parse_carps_header(data[:19])
        payload = data[19:19+carps_header.data_len+1]
        blocks.append((carps_header, payload))
        data = data[19+carps_header.data_len+1:]
    return blocks


def info_block_print(data):
    logger.debug("document magic bytes: %s", data[0:2])
    logger.debug("document magic bytes: %s", data[2:4])
    logger.debug("document magic bytes: %s", data[4:4+7])
    count_1 = data[4+7]
    logger.debug("document len=%s", count_1)
    logger.debug("filename name magic bytes: %s", data[4+7+1:4+7+1+2])
    count_2 = data[4+7+3]
    logger.debug("filename len=%s", count_2)
    logger.info("filename: %s", data[4+7+3+1:4+7+3+1+count_2])

    data = data[4+7+3+1+count_2:]

    logger.debug("username magic bytes: %s", data[0:2])
    logger.debug("username magic bytes: %s", data[2:4])
    logger.debug("username magic bytes: %s", data[4:6])
    count_3 = data[6]
    logger.debug("username len=%s", count_3)
    logger.info("username: %s", data[7:7+count_3])

    data = data[7+count_3:]

    logger.debug("time magic bytes: %s", data[0:2])
    logger.debug("time magic bytes: %s", data[2:4])
    time_print = data[4:]
    logger.debug("time: %s", time_print)
    year_mounts = time_print[0:2].hex()
    year = int(year_mounts[0:3], 16)
    mounth = int(year_mounts[3], 16)
    day_week = int.from_bytes(time_print[2:3], 'big')
    day = day_week >> 3
    week = int(bin(day_week)[7:], 2)
    hour = int.from_bytes(time_print[4:5], 'big')
    minutes = int.from_bytes(time_print[5:6], 'big')
    seconds = int.from_bytes(time_print[6:7], 'big') >> 2
    microsec = int(bin(int.from_bytes(time_print[6:8], 'big'))[8:], 2)
    logger.info("time: %s-%s-%s week=%s %s:%s:%s MILLISECONDS(?) %s ", 
                year, mounth, day, week, hour, minutes, seconds, microsec)


def main(args: argparse.Namespace):
    blocks_list = parse_blocks(args.infile.read())
    for header, data in blocks_list:
        if header.data_type == DataType.CONTROL and header.block_type == BlockType.DOC_INFO:
            info_block_print(data)
        elif header.data_type == DataType.CONTROL and header.block_type == BlockType.PARAMS:
            if data[2] == ord('\x2e'):
                logger.debug("%s %s", header.block_type, data)
            elif data[2] == ord('\x2d'):
                logger.info('ImageRefinement %s', ParamFlag(data[3]).name)
            elif data[2] == ord('\x5a'):
                logger.info('TonerSave %s', ParamFlag(data[3]).name)
            else:
                logger.error(f'{header.data_type}')
        elif header.data_type == DataType.CONTROL and header.block_type == BlockType.PRINT:
            logger.debug("%s %s %s", header.block_type, header.data_type, data)
        elif header.data_type == DataType.PRINT and header.block_type == BlockType.PRINT:
            parse_data_print_block(header, data)
        else:
            logger.debug("%s %s", header.block_type, header.data_type)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("infile", type=argparse.FileType("rb") )
    parser.add_argument("-log",
                        "--loglevel",
                        default="info",
                        help="Provide logging level. Example --loglevel debug, default=info" )

    args = parser.parse_args()

    stdout = logging.StreamHandler(stream=sys.stdout)
    logger.addHandler(stdout)

    levels = {
        'critical': logging.CRITICAL,
        'error': logging.ERROR,
        'warn': logging.WARNING,
        'warning': logging.WARNING,
        'info': logging.INFO,
        'debug': logging.DEBUG
    }
    logger.setLevel(levels.get(args.loglevel.lower()))

    main(args)
