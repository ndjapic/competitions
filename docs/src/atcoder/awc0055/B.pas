program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
type
	tpost = record
		x, c: int32;
	end;
var
	n, i, p, q, t, a: int32;
	ans: int32;
	posts: tlist<tpost>;
	post: tpost;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function compareposts(constref left, right: tpost): int32;
begin
	result := left.x - right.x;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, p, q);

	posts := tlist<tpost>.create;
	for i := 0 to n-1 do begin
		readln(post.x, post.c);
		posts.add(post);
		posts.exchange(i, random(i+1));
	end;
	posts.sort(tcomparer<tpost>.construct(compareposts));

	t := 0;
	a := 0;
	for i := 0 to n-1 do begin
		if abs(posts[i].x - p) < abs(posts[t].x - p) then t := i;
		if abs(posts[i].x - q) < abs(posts[a].x - q) then a := i;
	end;

	ans := posts[t].c + 2;
	if t <> a then inc(ans, posts[a].c);
	writeln(ans);
	posts.free;
end.
