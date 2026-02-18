import threading
import socket
import serial
import sqlite3
import time
import datetime
import sys
import logging
import threading
import uvicorn

# Configuration
logging.basicConfig(level=logging.INFO,
                    format="%(asctime)s [%(levelname)s] %(message)s")
DB_NAME = "log/messages.db"


def run_api():
    uvicorn.run("message_api:api", host="0.0.0.0", port=8000, reload=False)


def init_db():
    conn = sqlite3.connect(DB_NAME)
    c = conn.cursor()
    c.execute("""
        CREATE TABLE IF NOT EXISTS messages (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            source TEXT,
            timestamp TEXT,
            message TEXT
        );
    """)
    conn.commit()
    conn.close()


def log_message(source, message):
    timestamp = datetime.datetime.utcnow().isoformat()
    if 'GPGGA' in message:
        try:
            conn = sqlite3.connect(DB_NAME)
            c = conn.cursor()
            c.execute("INSERT INTO messages (source, timestamp, message) VALUES (?, ?, ?)",
                    (source, timestamp, message))
            conn.commit()
            conn.close()
        except Exception as e:
            logging.error(f"Failed to log message: {e}")


def serial_listener(port="/dev/ttyUSB0", baudrate=9600):
    while True:
        try:
            logging.info("Connecting to serial port...")
            ser = serial.Serial(port, baudrate, timeout=1)
            while True:
                line = ser.readline().decode(errors='ignore').strip()
                if line:
                    log_message("serial", line)
        except Exception as e:
            logging.warning(f"Serial error: {e}")
            time.sleep(5)  # Wait and retry


def tcp_listener(host='0.0.0.0', port=5000):
    while True:
        try:
            logging.info(f"Starting TCP server on {host}:{port}")
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                s.bind((host, port))
                s.listen()
                while True:
                    conn, addr = s.accept()
                    logging.info(f"TCP connection from {addr}")
                    with conn:
                        while True:
                            data = conn.recv(1024)
                            if not data:
                                break
                            log_message("tcp", data.decode(errors='ignore').strip())
        except Exception as e:
            logging.warning(f"TCP error: {e}")
            time.sleep(5)


def tcp_client_listener(server_ip='10.5.0.95', server_port=5000):
    while True:
        try:
            logging.info(f"Connecting to TCP server at {server_ip}:{server_port}")
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.connect((server_ip, server_port))
                s.settimeout(10)
                while True:
                    data = s.recv(1024)
                    if not data:
                        logging.warning("TCP connection closed by server.")
                        break
                    log_message("tcp", data.decode(errors='ignore').strip())
        except (socket.timeout, ConnectionRefusedError, OSError) as e:
            logging.warning(f"TCP client error: {e}")
            time.sleep(5)  # Retry after delay
            

def udp_listener(host='0.0.0.0', port=5001):
    while True:
        try:
            logging.info(f"Starting UDP server on {host}:{port}")
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
                s.bind((host, port))
                while True:
                    data, addr = s.recvfrom(1024)
                    log_message("udp", data.decode(errors='ignore').strip())
        except Exception as e:
            logging.warning(f"UDP error: {e}")
            time.sleep(5)


if __name__ == "__main__":
    init_db()

    # Threads for each input:
    threads = [
        #threading.Thread(target=serial_listener, kwargs={"port": "COM3"}, daemon=True),
        #threading.Thread(target=tcp_listener, kwargs={"port": 1000}, daemon=True),
        threading.Thread(target=udp_listener, kwargs={"port": 1457}, daemon=True),
        #threading.Thread(target=tcp_client_listener, kwargs={"server_port": 1000}, daemon=True),
        threading.Thread(target=run_api, daemon=True)
    ]

    for t in threads:
        t.start()

    logging.info("Logging started. Press Ctrl+C to stop.")

    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        logging.info("Shutting down.")
        sys.exit(0)
