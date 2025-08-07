import os
import time
from fastapi import FastAPI, Query
from typing import Optional, List
import sqlite3
import datetime

api = FastAPI()

DB_NAME = "log/messages.db"
START_TIME = time.time()


def extract_gpgga(nmea_line):
    _, sep, gpgga_part = nmea_line.partition("$GPGGA")
    return "$GPGGA" + gpgga_part if sep else None


def parse_gpgga(sentence):
    sentence = extract_gpgga(sentence) # Strip away any unnecessary characters or information.
    if not sentence.startswith("$GPGGA"):
        return {
            "timestamp": None,
            "latitude": None,
            "longitude": None,
            "fix_quality": None,
            "num_satellites": None,
            "hdop": None,
            "altitude_m": None,
        }
        #raise ValueError("Not a GPGGA sentence")

    # Strip checksum and split fields
    sentence = sentence.split("*")[0]  # Remove checksum
    parts = sentence.split(",")

    if len(parts) < 15:
        return {
            "timestamp": None,
            "latitude": None,
            "longitude": None,
            "fix_quality": None,
            "num_satellites": None,
            "hdop": None,
            "altitude_m": None,
        }
        raise ValueError("Incomplete GPGGA sentence")

    time_utc = parts[1]
    lat_raw = parts[2]
    lat_dir = parts[3]
    lon_raw = parts[4]
    lon_dir = parts[5]
    fix_quality = int(parts[6])
    num_satellites = int(parts[7])
    hdop = float(parts[8])
    altitude = float(parts[9])
    altitude_units = parts[10]

    # Convert time
    hh = int(time_utc[0:2])
    mm = int(time_utc[2:4])
    ss = float(time_utc[4:])
    timestamp = f"{hh:02}:{mm:02}:{ss:05.2f} UTC"

    # Convert latitude
    lat_deg = int(lat_raw[:2])
    lat_min = float(lat_raw[2:])
    latitude = lat_deg + lat_min / 60.0
    if lat_dir == "S":
        latitude = -latitude

    # Convert longitude
    lon_deg = int(lon_raw[:3])
    lon_min = float(lon_raw[3:])
    longitude = lon_deg + lon_min / 60.0
    if lon_dir == "W":
        longitude = -longitude

    return {
        "timestamp": timestamp,
        "latitude": latitude,
        "longitude": longitude,
        "fix_quality": fix_quality,
        "num_satellites": num_satellites,
        "hdop": hdop,
        "altitude_m": altitude,
    }


@api.get("/messages")
def get_messages(
    source: Optional[str] = None,
    contains: Optional[str] = None,
    since: Optional[str] = Query(None, description="ISO format timestamp (UTC)"),
    until: Optional[str] = Query(
        None, description="ISO format timestamp (UTC)"),
    limit: int = 100,
    by: Optional[int] = None

):
    query = "SELECT id, source, timestamp, message FROM messages WHERE 1=1"
    params = []

    if source:
        query += " AND source = ?"
        params.append(source)
    if contains:
        query += " AND message LIKE ?"
        params.append(f"%{contains}%")
    if since:
        query += " AND timestamp >= ?"
        params.append(since)
    if until:
        query += " AND timestamp <= ?"
        params.append(until)
    if by:
        query += " AND id % ? = 0"
        params.append(by)

    query += " ORDER BY timestamp DESC LIMIT ?"
    params.append(limit)

    conn = sqlite3.connect(DB_NAME)
    cursor = conn.cursor()
    cursor.execute(query, params)
    rows = cursor.fetchall()
    conn.close()

    return [
        {"id": r[0], "source": r[1], "timestamp": r[2], "message": r[3]}
        for r in rows
    ]


@api.get("/positions")
def get_positions(
    source: Optional[str] = None,
    contains: Optional[str] = "GPGGA",
    since: Optional[str] = Query(
        None, description="ISO format timestamp (UTC)"),
    until: Optional[str] = Query(
        None, description="ISO format timestamp (UTC)"),
    limit: int = 100,
    by: Optional[int] = None
):
    
    query = "SELECT id, source, timestamp, message FROM messages WHERE 1=1"
    params = []

    if source:
        query += " AND source = ?"
        params.append(source)
    if contains:
        query += " AND message LIKE ?"
        params.append(f"%{contains}%")
    if since:
        query += " AND timestamp >= ?"
        params.append(since)
    if until:
        query += " AND timestamp <= ?"
        params.append(until)
    if by:
        query += " AND id % ? = 0"
        params.append(by)

    query += " ORDER BY timestamp DESC LIMIT ?"
    params.append(limit)

    conn = sqlite3.connect(DB_NAME)
    cursor = conn.cursor()
    cursor.execute(query, params)
    rows = cursor.fetchall()
    conn.close()
    
    
    
    return [
        {"id": r[0], "source": r[1], "timestamp": r[2],
            "message": parse_gpgga(r[3])}
        for r in rows
    ]




@api.get("/stats")
def get_stats():
    conn = sqlite3.connect(DB_NAME)
    c = conn.cursor()

    c.execute("SELECT COUNT(*) FROM messages")
    total_messages = c.fetchone()[0]

    c.execute("SELECT source, COUNT(*) FROM messages GROUP BY source")
    messages_by_source = {row[0]: row[1] for row in c.fetchall()}

    c.execute(
        "SELECT id, source, timestamp, message FROM messages ORDER BY timestamp DESC LIMIT 1")
    latest = c.fetchone()

    c.execute("SELECT MIN(timestamp), MAX(timestamp) FROM messages")
    earliest, latest_time = c.fetchone()

    conn.close()

    return {
        "total_messages": total_messages,
        "messages_by_source": messages_by_source,
        "latest_message": {
            "id": latest[0],
            "source": latest[1],
            "timestamp": latest[2],
            "message": latest[3]
        } if latest else None,
        "earliest_timestamp": earliest,
        "latest_timestamp": latest_time,
        "uptime_seconds": round(time.time() - START_TIME, 2),
        "db_file_size_mb": round(os.path.getsize(DB_NAME) / 1024**2, 2)
    }


@api.get("/health")
def health():
    return {
        "running" : True,
        "status": "ok",
        "uptime_seconds": round(time.time() - START_TIME, 2)
    }


@api.get("/")
def landing_page():
    return {
        "name" : "Tom's Positional API",
        "description" : "This can be used to get the real-time or after-the-fact positions from a SQLite database populated with NMEA sentences."
    }
