import threading
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


if __name__ == "__main__":
    init_db()

    # Threads for each input:
    threads = [
        threading.Thread(target=run_api, daemon=True)
    ]

    for t in threads:
        t.start()

    logging.info("API (only) started. Press Ctrl+C to stop.")

    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        logging.info("Shutting down.")
        sys.exit(0)
