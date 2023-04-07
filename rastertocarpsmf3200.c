/* CUPS driver for Canon CARPS printers */
/* Copyright (c) 2014 Ondrej Zary */
/* Copyright (c) 2023 redjoe */
/* 
   Decoding algorithm CCITT Group 4 as T6, T.6, Fax4, G4, Group 4, or ITU-T Fax Group 4.
   libtiff
 * Copyright (c) 1990-1997 Sam Leffler
 * Copyright (c) 1991-1997 Silicon Graphics, Inc.
   https://gitlab.com/libtiff/libtiff/-/blob/master/libtiff/tif_fax3.c
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
// #include "tiffio.h"
#include <cups/ppd.h>
#include <cups/raster.h>
#include "carps.h"
#define G3CODES
#include "t4.h"

//#define DEBUG
#define PBM

#define ERR(fmt, args ...)	fprintf(stderr, "ERROR: CARPS " fmt "\n", ##args);
#define WARN(fmt, args ...)	fprintf(stderr, "WARNING: CARPS " fmt "\n", ##args);

#ifdef DEBUG
//#define DBG(fmt, args ...)	fprintf(stderr, "DEBUG: CARPS " fmt "\n", ##args);
#define DBG(fmt, args ...)	fprintf(stderr, fmt, ##args);
#else
#define DBG(fmt, args ...)	do {} while (0)
#endif

int global_line_num, global_outpos;

void fill_header(struct carps_header *header, u8 data_type, u8 block_type, int32_t data_len) {
	memset(header, 0, sizeof(struct carps_header));
	header->magic1 = 0xCD;
	header->magic2 = 0xCA;
	header->magic3 = 0x10;
	header->data_type = data_type;
	header->block_type = block_type;
	header->one = 0x01;
	header->data_len = cpu_to_be16(data_len);
}

void write_block(u8 data_type, u8 block_type, void *data, int32_t data_len, FILE *stream) {
	struct carps_header header;

	fill_header(&header, data_type, block_type, data_len);
	fwrite(&header, 1, sizeof(header), stream);
	fwrite(data, 1, data_len, stream);
	global_outpos += sizeof(header) + data_len;
}


static const int _msbmask[9] =
    { 0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff };


/*
 * Bit reversal tables.  TIFFBitRevTable[<byte>] gives
 * the bit reversed value of <byte>.  Used in various
 * places in the library when the FillOrder requires
 * bit reversal of byte values (e.g. CCITT Fax 3
 * encoding/decoding).  TIFFNoBitRevTable is provided
 * for algorithms that want an equivalent table that
 * do not reverse bit values.
 */
static const unsigned char TIFFBitRevTable[256] = {
    0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
    0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
    0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
    0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
    0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
    0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
    0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
    0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
    0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
    0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
    0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
    0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
    0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
    0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
    0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
    0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
    0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
    0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
    0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
    0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
    0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
    0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
    0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
    0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
    0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
    0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
    0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
    0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
    0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
    0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
    0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
    0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};


/* put n bits of data */
int Fax3PutBits(char **data, uint32_t *len, u8 *bitpos, unsigned int n, unsigned int bits) {
	if (!data) {
		//DBG("put bits: no data\n");
		return 0;
	}
	//DBG("put bits: bits=%d\n", bits);
	while (n > *bitpos) {
		*data[0] |= bits >> (n - *bitpos);	
		n -= *bitpos;
		*data[0] = TIFFBitRevTable[(unsigned int)*data[0]];
		//DBG("put bits in while: bits=%s  (reverse)\n", bin_n(*data[0], 8));
		(*data)++;
		(*len)++;
		*bitpos = 8;
	}
	*data[0] |= (bits & _msbmask[n]) << (*bitpos - n);
	*bitpos -= n;
	if (*bitpos == 0)
	{
		*data[0] = TIFFBitRevTable[(unsigned int)*data[0]];
		//DBG("put bits bitpos=0: bits=%s (reverse)\n", bin_n(*data[0], 8));
		*bitpos = 8;
		(*data)++;
		(*len)++;
	}
	return 1;
}


/* 
 * Write a code to the output stream.
 */
#define putcode(data, len, bitpos, te) Fax3PutBits(data, len, bitpos, (te)->length, (te)->code)


/*
 * Write the sequence of codes that describes
 * the specified span of zero's or one's.  The
 * appropriate table that holds the make-up and
 * terminating codes is supplied.
 */
static int
putspan(char **data, uint32_t *len, u8 *bitpos, int32_t span, const tableentry* tab)
{
	unsigned int code, length;
	/* DBG("span == %d \n", span ); */
	while (span >= 2624) {
		const tableentry* te = &tab[63 + (2560>>6)];
		code = te->code;
		length = te->length;
		Fax3PutBits(data, len, bitpos, length, code);
		span -= te->runlen;
		/* DBG("[span >= 2624]: span == %d, length= %d\n", span, length); */
	}
	if (span >= 64) {
		/* DBG("span == %d \n", span ); */
		const tableentry* te = &tab[63 + (span>>6)];
		/* DBG("assert!!!!!!!!!!!!! te->runlen == 64*(span>>6) is %d \n", te->runlen == 64*(span>>6) ); */
		code = te->code;
		length = te->length;
		
		/* DBG("[span >= 64]: span == %d, code=%d length= %d\n", span, code, length); */
		Fax3PutBits(data, len, bitpos, length, code);
		span -= te->runlen;
	}
	code = tab[span].code;
	length = tab[span].length;
	/* DBG("[span]: span == %d, code=%d length= %d\n", span, code, length); */
	Fax3PutBits(data, len, bitpos, length, code);

    return 1;
}


void Fax3FlushBits(char **data, uint32_t *len, u8 *bitpos) {
	*data[0] = TIFFBitRevTable[(unsigned int)*data[0]];
	*bitpos = 8;
	(*data)++;
	(*len)++;
}


u16 line_len, line_len_file;
int width, height, dpi;
u8 *last_lines[8], *cur_line;


#define isAligned(p,t) ((((size_t)(p)) & (sizeof (t)-1)) == 0)


static const unsigned char zeroruns[256] = {
    8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,	/* 0x00 - 0x0f */
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,	/* 0x10 - 0x1f */
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,	/* 0x20 - 0x2f */
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,	/* 0x30 - 0x3f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0x40 - 0x4f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0x50 - 0x5f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0x60 - 0x6f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0x70 - 0x7f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x80 - 0x8f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x90 - 0x9f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0xa0 - 0xaf */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0xb0 - 0xbf */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0xc0 - 0xcf */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0xd0 - 0xdf */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0xe0 - 0xef */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0xf0 - 0xff */
};
static const unsigned char oneruns[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x00 - 0x0f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x10 - 0x1f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x20 - 0x2f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x30 - 0x3f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x40 - 0x4f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x50 - 0x5f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x60 - 0x6f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0x70 - 0x7f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0x80 - 0x8f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0x90 - 0x9f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0xa0 - 0xaf */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0xb0 - 0xbf */
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,	/* 0xc0 - 0xcf */
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,	/* 0xd0 - 0xdf */
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,	/* 0xe0 - 0xef */
    4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8,	/* 0xf0 - 0xff */
};


static const tableentry horizcode =
    { 3, 0x1, 0 };	/* 001 */
static const tableentry passcode =
    { 4, 0x1, 0 };	/* 0001 */
static const tableentry vcodes[7] = {
    { 7, 0x03, 0 },	/* 0000 011 */
    { 6, 0x03, 0 },	/* 0000 11 */
    { 3, 0x03, 0 },	/* 011 */
    { 1, 0x1, 0 },	/* 1 */
    { 3, 0x2, 0 },	/* 010 */
    { 6, 0x02, 0 },	/* 0000 10 */
    { 7, 0x02, 0 }	/* 0000 010 */
};


/*
 * Find a span of ones or zeros using the supplied
 * table.  The ``base'' of the bit string is supplied
 * along with the start+end bit indices.
 */
static inline int32_t
find0span(unsigned char* bp, int32_t bs, int32_t be)
{
	int32_t bits = be - bs;
	int32_t n, span;

	bp += bs>>3;
	/*
	 * Check partial byte on lhs.
	 */
	if (bits > 0 && (n = (bs & 7)) != 0) {
		span = zeroruns[(*bp << n) & 0xff];
		if (span > 8-n)		/* table value too generous */
			span = 8-n;
		if (span > bits)	/* constrain span to bit range */
			span = bits;
		if (n+span < 8)		/* doesn't extend to edge of byte */
			return (span);
		bits -= span;
		bp++;
	} else
		span = 0;
	if (bits >= (int32_t)(2 * 8 * sizeof(int64_t))) {
		int64_t* lp;
		/*
		 * Align to int64_t boundary and check int64_t words.
		 */
		while (!isAligned(bp, int64_t)) {
			if (*bp != 0x00)
				return (span + zeroruns[*bp]);
			span += 8;
			bits -= 8;
			bp++;
		}
		lp = (int64_t*) bp;
		while ((bits >= (int32_t)(8 * sizeof(int64_t))) && (0 == *lp)) {
			span += 8*sizeof (int64_t);
			bits -= 8*sizeof (int64_t);
			lp++;
		}
		bp = (unsigned char*) lp;
	}
	/*
	 * Scan full bytes for all 0's.
	 */
	while (bits >= 8) {
		if (*bp != 0x00)	/* end of run */
			return (span + zeroruns[*bp]);
		span += 8;
		bits -= 8;
		bp++;
	}
	/*
	 * Check partial byte on rhs.
	 */
	if (bits > 0) {
		n = zeroruns[*bp];
		span += (n > bits ? bits : n);
	}
	return (span);
}

static inline int32_t
find1span(unsigned char* bp, int32_t bs, int32_t be)
{
	int32_t bits = be - bs;
	int32_t n, span;

	bp += bs>>3;
	/*
	 * Check partial byte on lhs.
	 */
	if (bits > 0 && (n = (bs & 7)) != 0) {
		span = oneruns[(*bp << n) & 0xff];
		if (span > 8-n)		/* table value too generous */
			span = 8-n;
		if (span > bits)	/* constrain span to bit range */
			span = bits;
		if (n+span < 8)		/* doesn't extend to edge of byte */
			return (span);
		bits -= span;
		bp++;
	} else
		span = 0;
	if (bits >= (int32_t)(2 * 8 * sizeof(int64_t))) {
		int64_t* lp;
		/*
		 * Align to int64_t boundary and check int64_t words.
		 */
		while (!isAligned(bp, int64_t)) {
			if (*bp != 0xff)
				return (span + oneruns[*bp]);
			span += 8;
			bits -= 8;
			bp++;
		}
		lp = (int64_t*) bp;
		while ((bits >= (int32_t)(8 * sizeof(int64_t))) && (~((uint64_t)0) == (uint64_t)*lp)) {
			span += 8*sizeof (int64_t);
			bits -= 8*sizeof (int64_t);
			lp++;
		}
		bp = (unsigned char*) lp;
	}
	/*
	 * Scan full bytes for all 1's.
	 */
	while (bits >= 8) {
		if (*bp != 0xff)	/* end of run */
			return (span + oneruns[*bp]);
		span += 8;
		bits -= 8;
		bp++;
	}
	/*
	 * Check partial byte on rhs.
	 */
	if (bits > 0) {
		n = oneruns[*bp];
		span += (n > bits ? bits : n);
	}
	return (span);
}


/*
 * Return the offset of the next bit in the range
 * [bs..be] that is different from the specified
 * color.  The end, be, is returned if no such bit
 * exists.
 */
#define	finddiff(_cp, _bs, _be, _color)	\
	(_bs + (_color ? find1span(_cp,_bs,_be) : find0span(_cp,_bs,_be)))
/*
 * Like finddiff, but also check the starting bit
 * against the end in case start > end.
 */
#define	finddiff2(_cp, _bs, _be, _color) \
	(_bs < _be ? finddiff(_cp,_bs,_be,_color) : _be)



static int Fax3Encode2DRow(char **data, uint32_t *len, u8 *bitpos, 
                           unsigned char* cur_line, unsigned char* last_line, uint32_t width)
{
#define	PIXEL(buf,ix)	((((buf)[(ix)>>3]) >> (7-((ix)&7))) & 1)
	uint32_t a0 = 0;
	uint32_t a1 = (PIXEL(cur_line, 0) != 0 ? 0 : finddiff(cur_line, 0, width, 0));
	uint32_t b1 = (PIXEL(last_line, 0) != 0 ? 0 : finddiff(last_line, 0, width, 0));
	uint32_t a2, b2;
	for (;;) {
		b2 = finddiff2(last_line, b1, width, PIXEL(last_line,b1));
		//DBG("a1=%d, b1=%d, b2=%d\n", a1, b1, b2);
		if (b2 >= a1) {
			/* Naive computation triggers -fsanitize=undefined,unsigned-integer-overflow */
			/* although it is correct unless the difference between both is < 31 bit */
			/* int32_t d = b1 - a1; */
			int32_t d = (b1 >= a1 && b1 - a1 <= 3U) ? (int32_t)(b1 - a1) :
						(b1 < a1 && a1 - b1 <= 3U) ? -(int32_t)(a1 - b1) : 0x7FFFFFFF;
			if (!(-3 <= d && d <= 3)) { /* horizontal mode */
				a2 = finddiff2(cur_line, a1, width, PIXEL(cur_line,a1));
				if( !putcode(data, len, bitpos, &horizcode))
                    return 0;
				if (a0+a1 == 0 || PIXEL(cur_line, a0) == 0) {
					if( !putspan(data, len, bitpos, a1-a0, TIFFFaxWhiteCodes))
						return 0;
					if( !putspan(data, len, bitpos, a2-a1, TIFFFaxBlackCodes))
						return 0;
				} else {
					//DBG("a0=a1 else a1-a0=%d a2-a1=%d\n", a1-a0,  a2-a1);
					if( !putspan(data, len, bitpos, a1-a0, TIFFFaxBlackCodes))
						return 0;
					if( !putspan(data, len, bitpos, a2-a1, TIFFFaxWhiteCodes))
						return 0;
				}
				a0 = a2;
			} else { /* vertical mode */
				//DBG("d=%d, vertical mode\n",  d);
				if (!putcode(data, len, bitpos, &vcodes[d+3]))
					return 0;
				a0 = a1;
			}	
		} else {						/* pass mode */
			//DBG("pass mode b2=%d >= a1=%d\n",  b2, a1);
			if (! putcode(data, len, bitpos, &passcode))
				return 0;
			a0 = b2;
		}
		if (a0 >= width)
			break;
		a1 = finddiff(cur_line, a0, width, PIXEL(cur_line,a0));
		b1 = finddiff(last_line, a0, width, !PIXEL(cur_line,a0));
		b1 = finddiff(last_line, b1, width, PIXEL(cur_line,a0));
	}
	return (1);
#undef PIXEL
}


uint32_t encode_print_data(int *num_lines, bool last, FILE *f, cups_raster_t *ras, char *out) {
	u8 bitpos = 8;
	uint32_t len = 0;
	int line_num = 0;
	DBG("num_lines=%d\n", *num_lines);
	//DBG("width=%d\n", width);

#ifdef DEBUG
	char *start = out;
#endif
	/*
		White reference line
	*/
	memset(last_lines[0], 0x0, line_len);

	while (((f && !feof(f)) || (ras)) && line_num < *num_lines) {
		memset(cur_line, 0, line_len);
		DBG("line(%d, %d) len=%d \n", line_num, *num_lines, len);

		if (ras) {
			/*
			* Read a line of graphics...
			*/
			DBG("cupsRasterReadPixels(%p, %p, %d)\n", ras, cur_line, line_len_file);
			if (cupsRasterReadPixels(ras, cur_line, line_len_file) == 0) {
				DBG("cupsRasterReadPixels break \n");
				break;
			}
		} else 
			fread(cur_line, 1, line_len_file, f);
		if( !Fax3Encode2DRow(&out, &len, &bitpos, cur_line, last_lines[0], width) ){
			DBG("!Fax3Encode2DRow return 0\n");
			return (0);
		}
		//DBG("line number =%d \n", line_num);
		memcpy(last_lines[1], last_lines[0], line_len);
		memcpy(last_lines[0], cur_line, line_len);
		line_num++;
		global_line_num++;
	}
	/* block end marker */
	if (last) {
		DBG("islast EOL end\n");
		Fax3PutBits(&out, &len, &bitpos, 12, EOL);
		Fax3PutBits(&out, &len, &bitpos, 12, EOL);
	}
	/* fill unused bits in last byte */
	if (bitpos != 8)
		DBG("Fax3FlushBits\n");
		Fax3FlushBits(&out, &len, &bitpos);

	*num_lines = line_num;

	return len;
}

int encode_print_block(int height, FILE *f, cups_raster_t *ras) {
	int num_lines = 65536;
	unsigned int ofs;
	bool last = false;
	char buf[BUF_SIZE];
	char *buf2;
	buf2 = (char*)malloc(height * 4720);  // TODO: fix. This need dynamic allocated buffer

	if (num_lines > height) {
		num_lines = height;
		last = true;
	}
	/* encode print data first as we need the length and line count */
	uint32_t len = encode_print_data(&num_lines, last, f, ras, buf2);
	DBG("len = %d\n", len);
	
	/* strip header */
	ofs = sprintf(buf, "\x01\x1b[;%d;%d;16.P", width, num_lines);
	/* print data header */
	struct carps_print_header *ph = (void *)buf + ofs;
	memset(ph, 0, sizeof(struct carps_print_header));
	ph->one = 0x01;
	ph->two = 0x02;
	ph->four = 0x04;
	ph->eight = 0x08;
	ph->magic = 0x50;
	ph->last = last ? 0 : 1;
	ph->data_len = cpu_to_le16(len);
	
	if (len <= MAX_DATA_LEN - ofs) {
		DBG("write_block len<=MAX_DATA_LENif len = %d, MAX_DATA_LEN=%d\n", len, MAX_DATA_LEN);

		/* copy print data after the headers */
		memcpy(buf + ofs, buf2, len);
		len = ofs + len;

		write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, buf, len, stdout);
	}
	else {
		DBG("write_block len<=MAX_DATA_LENelse len = %d, MAX_DATA_LEN=%d\n", len, MAX_DATA_LEN);
		/* write strip header + print data header separately */
		write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, buf, ofs, stdout);
		
		/* then write data blocks at most MAX_BLOCK_LEN bytes long */
		int temp_ls = 0;
		while (len) {
			/* insert 0x01 byte at the beginning of each continuing block */
			DBG("start; len = %d, temp_ls=%d\n", len, temp_ls);
			DBG("buf2 = %p; %p\n", &buf2, buf2+temp_ls);
			buf[0] = 0x01;
			int block_len = (len > MAX_DATA_LEN) ? MAX_DATA_LEN - 1 : len;
			memcpy(buf+1, buf2 + temp_ls, block_len);
			fprintf(stderr, "len=%d, block_len=%d\n", len, block_len);
			write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, buf, block_len + 1, stdout);
			len -= block_len;
			temp_ls += block_len;
			DBG("len = %d, temp_ls=%d\n", len, temp_ls);
		}
	}

	return num_lines;
}

enum carps_paper_size encode_paper_size(const char *paper_size_name) {
	if (!strcmp(paper_size_name, "A4"))
		return PAPER_A4;
	else if (!strcmp(paper_size_name, "A5"))
		return PAPER_A5;
	else if (!strcmp(paper_size_name, "B5"))
		return PAPER_B5;
	else if (!strcmp(paper_size_name, "Letter"))
		return PAPER_LETTER;
	else if (!strcmp(paper_size_name, "Legal"))
		return PAPER_LEGAL;
	else if (!strcmp(paper_size_name, "Executive"))
		return PAPER_EXECUTIVE;
	else if (!strcmp(paper_size_name, "Monarch"))
		return PAPER_ENV_MONAR;
	else if (!strcmp(paper_size_name, "Env10"))
		return PAPER_ENV_COM10;
	else if (!strcmp(paper_size_name, "DL"))
		return PAPER_ENV_DL;
	else if (!strcmp(paper_size_name, "C5"))
		return PAPER_ENV_C5;
	else
		return PAPER_CUSTOM;
}

void fill_print_data_header(char *buf, unsigned int copies, unsigned int dpi, unsigned int weight, const char *paper_size_name, unsigned int paper_width, unsigned int paper_height) {
	char tmp[100];
	enum carps_paper_size paper_size = encode_paper_size(paper_size_name);

	buf[0] = 1;
	buf[1] = 0;
	strcat(buf, "\x1b%@");
	/* ??? and resolution */
	sprintf(tmp, "\x1bP42;%d;1J;ImgColor", dpi);
	strcat(buf, tmp);
	/* ??? */
	strcat(buf, "\x1b\\");
	/* ??? */
	strcat(buf, "\x1b[11h");
	/* ??? and resolution */
	sprintf(tmp, "\x1b[?7;%d I", dpi);
	strcat(buf, tmp);
	/* paper weight */
	sprintf(tmp, "\x1b[%d't", weight);
	strcat(buf, tmp);
	/* paper size */
	if (paper_size == PAPER_CUSTOM) {
		/* compute custom paper size in print dots */
		int margin = 2 * dpi / 10;
		paper_height = paper_height * dpi / POINTS_PER_INCH;
		paper_width = paper_width * dpi / POINTS_PER_INCH;
		sprintf(tmp, "\x1b[%d;%d;%d;%d;%d;%d;%dp", paper_size, paper_height, paper_width, margin, margin, margin, margin);
	} else
		sprintf(tmp, "\x1b[%d;;;;;;p", paper_size);
	strcat(buf, tmp);
	/* ??? */
	strcat(buf, "\x1b[?2h");
	/* number of copies */
	sprintf(tmp, "\x1b[%dv", copies);
	strcat(buf, tmp);
	/* resolution and ??? */
	sprintf(tmp, "\x1b[%d;1;0;256;;0;0'c", dpi);
	strcat(buf, tmp);
}

char *ppd_get(ppd_file_t *ppd, const char *name) {
	ppd_attr_t *attr = ppdFindAttr(ppd, name, NULL);

	if (attr)
		return attr->value;
	else {
		ppd_choice_t *choice;
		choice = ppdFindMarkedChoice(ppd, name);
		if (!choice)
			return NULL;
		return choice->choice;
	}
}

int main(int argc, char *argv[]) {
	char buf[BUF_SIZE];
	struct carps_doc_info_mf3200 *info;
	struct carps_doc_user_mf3200 *user;
	struct carps_time_mf3200 *doc_time;
	struct carps_print_params params;
	char tmp[100];
#ifdef PBM
	bool pbm_mode = false;
#else
#define pbm_mode 0
#endif
	FILE *f;
	cups_raster_t *ras = NULL;
	cups_page_header2_t page_header;
	unsigned int page = 0, copies;
	int fd;
	ppd_file_t *ppd;
	bool header_written = false;
#ifdef PBM
	if (argc < 2 || argc == 3 || argc == 4 || argc == 5 || argc > 7) {
		fprintf(stderr, "usage: rastertocarps <file.pbm>\n");
#else
	if (argc < 6 || argc > 7) {
#endif
		fprintf(stderr, "usage: rastertocarps job-id user title copies options [file]\n");
		return 1;
	}
#ifdef PBM
	if (argc < 3)
		pbm_mode = true;
#endif
	if (pbm_mode) {
		f = fopen(argv[1], "r");
		if (!f) {
			perror("Unable to open file");
			return 2;
		}

		fgets(tmp, sizeof(tmp), f);
		if (strcmp(tmp, "P4\n")) {
			fprintf(stderr, "Invalid PBM file\n");
			return 2;
		}
		do
			fgets(tmp, sizeof(tmp), f);
		while (tmp[0] == '#');
		sscanf(tmp, "%d %d", &width, &height);
		DBG("width=%d height=%d\n", width, height);
		line_len_file = width;
		line_len = width;
		cur_line = malloc(line_len);
		for (int i = 0; i < 8; i++)
			last_lines[i] = malloc(line_len);
	} else {
		int n;
		cups_option_t *options;

		copies = atoi(argv[4]);
		if (copies < 1)
			copies = 1;

		if (argc > 6) {
			fd = open(argv[6], O_RDONLY);
			if (fd == -1) {
				perror("ERROR: Unable to open raster file - ");
				return 1;
			}
		} else
			fd = 0;
		ras = cupsRasterOpen(fd, CUPS_RASTER_READ);
		ppd = ppdOpenFile(getenv("PPD"));
		if (!ppd) {
			fprintf(stderr, "Unable to open PPD file %s\n", getenv("PPD"));
			return 2;
		}
		ppdMarkDefaults(ppd);
		n = cupsParseOptions(argv[5], 0, &options);
		cupsMarkOptions(ppd, n, options);
		cupsFreeOptions(n, options);
	}

	/* document info - title */
	char *doc_title;
	if (pbm_mode)
		doc_title = "Untitled";
	else
		doc_title = argv[3];
	info = (void *)buf;
	info->type = cpu_to_be16(CARPS_DOC_INFO_TITLE);
	info->unknown = cpu_to_be16(0xF0);
	info->unknown1 = cpu_to_be16(0x01);
	info->unknown2 = cpu_to_be16(0x0100);
	info->unknown3 = cpu_to_be16(0x0400);
	info->data_len = strlen(doc_title) > 255 ? 255 : strlen(doc_title) + 3;  // TODO: maybe more 255
	info->unknown4 = cpu_to_be16(0x11);
	info->data_len1 = strlen(doc_title) > 255 ? 255 : strlen(doc_title);
	strncpy(buf + sizeof(struct carps_doc_info_mf3200), doc_title, 255);

	/* document info - user name */
	char *user_name;
	if (pbm_mode)
	 	user_name = "user";
	else
		user_name = argv[2];
	user = (void *)buf + sizeof(struct carps_doc_info_mf3200) + strlen(doc_title);
	user->type = cpu_to_be16(CARPS_DOC_INFO_USER);
	user->unknown = cpu_to_be16(0x07);
	user->unknown1 = cpu_to_be16(0x11);
	user->data_len = strlen(user_name) > 255 ? 255 : strlen(user_name);
	strncpy(buf + sizeof(struct carps_doc_info_mf3200) + sizeof(struct carps_doc_user_mf3200) + strlen(doc_title), user_name, 255);
	
	/* document info - time */
	time_t timestamp = time(NULL);
	struct tm *tm = gmtime(&timestamp);
	doc_time = (void *)buf + sizeof(struct carps_doc_info_mf3200) + sizeof(struct carps_doc_user_mf3200) + strlen(doc_title) + strlen(user_name);
	memset(doc_time, 0, sizeof(struct carps_time_mf3200));
	doc_time->type = cpu_to_be16(CARPS_DOC_INFO_TIME);
	doc_time->unknown = cpu_to_be16(0x08);
	if (!pbm_mode) {
		doc_time->year = (1900 + tm->tm_year) >> 4;
		doc_time->year_month = ((1900 + tm->tm_year) << 4) | (tm->tm_mon + 1);
		doc_time->day = (tm->tm_mday << 3) | tm->tm_wday;
		doc_time->hour = tm->tm_hour;
		doc_time->min = tm->tm_min;
		doc_time->sec_msec = tm->tm_sec << 2;
	} else {
		/* MF5730 driver does not fill the time data, maybe because of a bug? */
		/* Printer accepts data with time so we always fill it in. */
		/* But we use this for test purposes so the output file does not change with time */
		doc_time->day = 7;
	}
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_DOC_INFO_MF3200, buf, sizeof(struct carps_doc_info_mf3200) + sizeof(struct carps_doc_user_mf3200) + strlen(doc_title) + strlen(user_name) + sizeof(struct carps_time_mf3200), stdout);
	/* begin 1 */
	memset(buf, 0, 4);
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_BEGIN1, buf, 4, stdout);
	/* begin 2 */
	memset(buf, 0, 4);
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_BEGIN2, buf, 4, stdout);
	/* print params - unknown  */
	u8 unknown_param[] = { 0x00, 0x2e, 0x82, 0x00, 0x00 };
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_PARAMS, unknown_param, sizeof(unknown_param), stdout);
	/* print params - image refinement */
	params.magic = CARPS_PARAM_MAGIC;
	params.param = CARPS_PARAM_IMAGEREFINE;
	params.enabled = CARPS_PARAM_ENABLED;
	if (!pbm_mode) {
		char *value = ppd_get(ppd, "ImageRefinement");
		if (!strcmp(value, "OFF"))
			params.enabled = CARPS_PARAM_DISABLED;
	}
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_PARAMS, &params, sizeof(params), stdout);
	/* print params - toner save */
	params.magic = CARPS_PARAM_MAGIC;
	params.param = CARPS_PARAM_TONERSAVE;
	params.enabled = CARPS_PARAM_DISABLED;
	if (!pbm_mode) {
		char *value = ppd_get(ppd, "TonerSave");
		/* if (strcmp(value, "DEFAULT"))
		 	params.enabled = CARPS_PARAM_DISABLED; */
		if (!strcmp(value, "ON"))
			params.enabled = CARPS_PARAM_ENABLED;
	}
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_PARAMS, &params, sizeof(params), stdout);
	
	if (!pbm_mode) {
		while (cupsRasterReadHeader2(ras, &page_header)) {
			page++;
			fprintf(stderr, "PAGE: %d %d\n", page, page_header.NumCopies);

			line_len_file = page_header.cupsBytesPerLine;
			line_len = line_len_file;
			if (!cur_line) {
				cur_line = malloc(line_len);
				for (int i = 0; i < 8; i++)
					last_lines[i] = malloc(line_len);
			}
			height = page_header.cupsHeight;
			width = page_header.cupsWidth;
			dpi = page_header.HWResolution[0];
			DBG("line_len_file=%d,line_len=%d height=%d width=%d\n", line_len_file, line_len, height, width);
			if (!header_written) {	/* print data header */
				char *page_size_name = page_header.cupsPageSizeName;
				/* get page size name from PPD if cupsPageSizeName is empty */
				if (strlen(page_size_name) == 0)
					page_size_name = ppd_get(ppd, "PageSize");
				fill_print_data_header(buf, copies, dpi, page_header.cupsMediaType, page_size_name, page_header.PageSize[0], page_header.PageSize[1]);
				write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, buf, strlen(buf), stdout);
				header_written = true;
			}

			/* read raster data */
			while (height > 0)
				height -= encode_print_block(height, NULL, ras);
			/* end of page */
			u8 page_end[] = { 0x01, 0x0c };
			write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, page_end, sizeof(page_end), stdout);
			DBG("end page %d\n", page);
		}
	} else {
		/* print data header */
		fill_print_data_header(buf, 1, 600, WEIGHT_PLAIN, "A4", 0, 0);	/* 1 copy, 600 dpi, plain paper, A4 */
		write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, buf, strlen(buf), stdout);
		/* print data */
		while (!feof(f) && height > 0)
			height -= encode_print_block(height, f, NULL);
		/* end of page */
		u8 page_end[] = { 0x01, 0x0c };
		write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, page_end, sizeof(page_end), stdout);
	}
	if (pbm_mode)
		fclose(f);
	else {
		ppdClose(ppd);
		cupsRasterClose(ras);
	}
	/* end of print data */
	u8 print_data_end[] = { 0x01, 0x1b, 'P', '0', 'J', 0x1b, '\\' };
	write_block(CARPS_DATA_PRINT, CARPS_BLOCK_PRINT, print_data_end, sizeof(print_data_end), stdout);
	/* end of print data */
	buf[0] = 1;
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_PRINT, buf, 1, stdout);
	/* end 2 */
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_END2, NULL, 0, stdout);
	/* end 1 */
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_END1, NULL, 0, stdout);
	/* end of document */
	DBG("End of document\n");
	buf[0] = 0;
	write_block(CARPS_DATA_CONTROL, CARPS_BLOCK_END, buf, 1, stdout);

	if (cur_line) {
		free(cur_line);
		for (int i = 0; i < 8; i++)
			free(last_lines[i]);
	}

	return 0;
}
