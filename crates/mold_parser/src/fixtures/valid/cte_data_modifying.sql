WITH inserted AS (
    INSERT INTO logs (msg) VALUES ('test') RETURNING id
)
SELECT * FROM inserted;
