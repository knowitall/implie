
import sys

lines = file(sys.argv[1], 'r').read().splitlines()
lines = [l for l in lines if len(l.strip()) > 0]
out = file(sys.argv[2], 'w')
out.write('\n'.join(lines))
out.close()
