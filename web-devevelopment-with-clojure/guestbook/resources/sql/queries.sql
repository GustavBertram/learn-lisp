-- :name save-message! :! :n
-- :doc creates a new message using the name, message and timestamp keys
INSERT INTO guestbook
(name, message, timestamp)
VALUES (:name, :message, :timestamp)

-- :name get-messages :? :*
-- :doc get all messages
SELECT *
FROM guestbook
