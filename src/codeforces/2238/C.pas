program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults,
	math;
const
	NN = 200 * 1000;
var
	notc, tci, n, u, v, i, ans: int32;
	p: array [2 .. NN] of int32;
	c: array [1 .. NN] of tlist<int32>;
	h: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ChildCompare(constref l, r: int32): int32;
begin
	result := CompareValue(h[r], h[l]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for u := 1 to n do begin
			c[u] := tlist<int32>.create(TComparer<int32>.Construct(ChildCompare));
			h[u] := 0;
		end;

		for v := 2 to n do begin
			read(u);
			p[v] := u;
			i := c[u].count;
			c[u].add(v);
			c[u].exchange(i, random(i+1))
		end;
		readln;

		for v := n downto 2 do begin
			u := p[v];
			h[u] := max(h[u], h[v] + 1);
		end;

		ans := n;
		for u := 1 to n do
			if c[u].count > 1 then begin
				c[u].sort;
				v := c[u][1];
				inc(ans, h[v] + 1);
			end;

		writeln(ans);
		for u := 1 to n do c[u].free;

	end;
end.
