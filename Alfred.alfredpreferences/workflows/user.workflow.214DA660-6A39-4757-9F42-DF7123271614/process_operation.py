#! /usr/local/bin/python
import sys
import logging
import volume_control as v

def process_operation(q):
    if len(q) == 0:
        return
    op = q[0]
    old = int(v.get_volume())
    new = old
    if op == 'mute':
        v.set_muted('true')
    elif op == 'unmute':
        v.set_muted('false')
    else:
        if op == 'up':
            delta = int(q[1]) if len(q) >= 2 else 10
            new = min(old + delta, 100)
        elif op == 'down':
            delta = int(q[1]) if len(q) >= 2 else 10
            new = max(old - delta, 0)
        else:
            try:
                new = int(op)
            except:
                new = old
            new = max(new, 0)
            new = min(new, 100)
        logging.debug(new)
        v.set_volume(new)
        return new


if __name__ == "__main__":
    process_operation(sys.argv[1:])