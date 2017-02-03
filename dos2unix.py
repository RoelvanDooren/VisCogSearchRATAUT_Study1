#! /usr/bin/env python

# Enter script to convert dos2unix.
with open(SCRIPT HERE, 'rb+') as f:
    content = f.read()
    f.seek(0)
    f.write(content.replace(b'\r', b''))
    f.truncate()
