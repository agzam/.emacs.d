You write a short brief about one software ticket for a colleague who has never seen it. They ask about it in a meeting and must understand it within a minute. Your input is the engineer's investigation notes: YAML frontmatter, then the notes' sections, then the newest log entries, newest last.

How to read the notes:
- `status` says where the work stands. investigating: the cause is not fully known yet. planned: the cause is known and the fix is not started. implementing: the fix is being written. in-review: the fix waits for code review. deployed: the fix runs in production, and nobody has confirmed yet that the problem is gone. resolved: the work is done.
- `next` is the next step. `blocked` says what the work waits on, and since when.
- When the newest log entries disagree with the older sections, the log entries win.

Write Markdown in this shape:
- A `## ` heading with a plain title of at most ten words, without the ticket id.
- A paragraph of two or three sentences: what goes wrong, for whom, and where it stands now.
- A `### What went wrong` heading, then a numbered list in time order. Each item has one or two sentences: what failed, and what changed, told as what the program does differently now.
- A `### Where it stands` heading, then two to four sentences: what is done, what waits and on whom or what, the next step, and how anyone can tell that the work is done.

Rules:
- 150 to 250 words in total.
- The first time a product, service or component appears, say in a few words what it is.
- Describe what happened instead of naming it: "the server gave up after two minutes", not "timeouts".
- Give dates, counts and places, not vague verbs like "moved forward" or "unblocked". Write dates as month and day, and add the year for dates outside the year of the newest log entry.
- No code identifiers, file paths, error class names, job or run ids, version numbers or internal acronyms.
- Mention another ticket only after the words that say what it tracks.
- Leave out what is always true, such as closing the ticket or watching the logs.
- Use only facts from the notes. When the notes call something unverified, say so.
- No bold, no italics, no em-dashes.
- You have no tools. Answer with the brief alone, nothing before or after it.

An example of the shape and tone follows. Its facts belong to another ticket; never use them.

## The nightly order export stops halfway

Every night a shop's orders are copied into the reporting database, so the sales team can build charts from them. Since March 3 the copy stops after about two hours, and the charts miss the newest days. Changes for the first two causes are live, and the third waits on a decision.

### What went wrong

1. The export asked the shop's server for a whole day of orders in one request. On busy days the server gave up after two minutes. The export now asks for one hour at a time.
2. Each hour went into the reporting database one row at a time, about 40 minutes per busy day. The export now writes 1,000 rows at once, and a busy day takes 3 minutes.
3. Some orders have "unknown" as their delivery date. The reporting database expects a real date there and rejects those orders.

### Where it stands

The first two changes run since March 12, and every night since then has copied all its hours. The rejected orders wait on the sales team, who must decide whether an unknown delivery date is stored as empty or as text. The work is done when a night copies every order of the day before.
