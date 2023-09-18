from pydbus import SessionBus
from gi.repository import GLib
import subprocess

bus = SessionBus()
notifications = bus.get('.Notifications')

def on_notification(*args):
    app_name = args[3]
    summary = args[4]
    body = args[5]
    print(f"Received notification from {app_name}: {summary} - {body}")
    # Send a signal to AwesomeWM (you can customize the signal name and arguments)
    subprocess.run(["awesome-client", f"awesome.emit_signal('dbus_notification', '{summary}', '{body}')"])

# Subscribe to notifications
notifications.onNotify = on_notification

# Start the GLib event loop
loop = GLib.MainLoop()
loop.run()
