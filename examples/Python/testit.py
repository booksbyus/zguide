import os

CHUNK_SIZE = 250000

def main():
    file = open("testdata", "rb")

    offset = 0

    while True:
        file.seek(offset, os.SEEK_SET)
        data = file.read(CHUNK_SIZE)
        if not data:
            break
        offset += len(data)

    file.close()

if __name__ == '__main__':
    main()
