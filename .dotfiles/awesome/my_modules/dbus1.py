from pydbus import SessionBus

bus = SessionBus()

# Make sure we're connected to the session bus
print("Connected to D-Bus Session")

notifications = bus.get('.Notifications')

def onNotify(*args):
    print("Notification received")
    print(args)

# Subscribe to notifications
print("Subscribing to notifications...")
bus.subscribe(iface='org.freedesktop.Notifications', signal='Notify', signal_fired=onNotify)

# Keep the program running
import time
while True:
    time.sleep(1)
