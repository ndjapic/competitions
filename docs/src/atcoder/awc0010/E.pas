program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #mask #seen #dfs #permutation
uses
	math;
const
	nn = 8;
var
	n, i, j, k: int8;
	ans: int32;
	p: array [1 .. nn] of int8;
	c: array [1 .. nn, 1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function effect(): int32;
var
	i: int8;
begin
	result := c[p[n], p[1]];
	for i := 2 to n do inc(result, c[p[i-1], p[i]]);
end;

function dist(): int8;
var
	i, j: int8;
	mask: int16;
begin
	result := 0;
	mask := 0;
	for i := 1 to n do
		if not odd(mask shr i) then begin
			j := i;
			while not odd(mask shr j) do begin
				inc(mask, 1 shl j);
				j := p[j];
				inc(result);
			end;
			dec(result);
		end;
end;

procedure dfs(i: int8; mask: int16);
var
	j: int8;
begin
	if i > 0 then begin
		for j := 1 to n do
			if not odd(mask shr j) then begin
				p[i] := j;
				dfs(i-1, mask + (1 shl j));
			end;
	end else if dist() <= k then
		ans := max(ans, effect());
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do begin
		for j := 1 to n do read(c[i, j]);
		readln;
		p[i] := i;
	end;

	ans := 0;
	dfs(n, 0);
	writeln(ans);
end.
