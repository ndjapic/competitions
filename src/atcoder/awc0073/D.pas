program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unfinished
uses
	generics.collections;
const
	MM = 200;
type
	troad = record
		v, w: int32;
	end;
var
	n, m, k, i, u, v, w, s, t: int32;
	adj: array [1 .. MM] of tlist<troad>;
	dist: array [1 .. MM, 1 .. MM] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for u := 1 to m do adj[u] := tlist<troad>.create;

	for i := 1 to k do begin
		readln(u, v, w);
		
	end;

	for u := 1 to m do adj[u].free;
end.
