import sqlite3
conn = sqlite3.connect('money2020.db')

c = conn.cursor()

def create_user(username, password):
    purchases = []
    payment_methods = []
    user = (username, password, payment_methods, purchases)
    c.execute('INSERT INTO users VALUES (?,?,?,?)', user)
