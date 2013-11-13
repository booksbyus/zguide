"""UDP ping command
Model 3, uses abstract network interface
"""

from interface import Interface

def main():
    interface = Interface()
    while True:
        try:
            print(interface.recv())
        except KeyboardInterrupt:
            print("interrupted")
            break
    interface.stop()

if __name__ == '__main__':
    main()