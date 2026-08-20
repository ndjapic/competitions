program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #joke
var
	n, i: int8;
	k, w, s, tw, ts: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	tw := 0;
	ts := 0;

	for i := 1 to n do begin
		readln(w, s);
		inc(tw, w);
		inc(ts, s);
	end;

	if tw < k then ts := -1;
	writeln(ts);
end.
