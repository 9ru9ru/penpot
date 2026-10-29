-- penpot-ko fork: emoji reactions on comments.
CREATE TABLE comment_reaction (
  comment_id uuid NOT NULL REFERENCES comment(id) ON DELETE CASCADE DEFERRABLE,
  profile_id uuid NOT NULL REFERENCES profile(id) ON DELETE CASCADE DEFERRABLE,
  emoji text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),

  PRIMARY KEY (comment_id, profile_id, emoji)
);

CREATE INDEX comment_reaction__profile_id__idx
    ON comment_reaction (profile_id);
