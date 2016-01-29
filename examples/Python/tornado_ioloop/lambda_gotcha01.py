from functools import partial


def f(arg):
    print('    arg: {}'.format(arg))


def test():
    a = ['one', 'two', 'three']
    b = [lambda: f(x) for x in a]
    print('lambda:')
    b[0]()
    b[1]()
    b[2]()
    print('-' * 40)
    b = [partial(f, x) for x in a]
    print('partial:')
    b[0]()
    b[1]()
    b[2]()


test()
