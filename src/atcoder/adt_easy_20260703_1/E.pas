program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #matrix #submatrix #search #combinations #bitmask #dfs
const
	HH = 10;
var
	h1, w1, h2, w2, i, j: int8;
	ans: boolean;
	a, b: array [1 .. HH, 1 .. HH] of int32;
	r, c: array [0 .. HH] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs2(j: int8);
var
	i: int8;
begin
	if not ans then begin
		if j <= w2 then begin

			c[j] := c[j-1] + 1;
			while c[j] <= w1 - (w2 - j) do begin
				dfs2(j+1);
				inc(c[j]);
			end;

		end else begin

			ans := true;
			for i := 1 to h2 do
				for j := 1 to w2 do
					ans := ans and (a[ r[i], c[j] ] = b[i, j]);

		end;
	end;
end;

procedure dfs1(i: int8);
begin
	if not ans then begin
		if i <= h2 then begin

			r[i] := r[i-1] + 1;
			while r[i] <= h1 - (h2 - i) do begin
				dfs1(i+1);
				inc(r[i]);
			end;

		end else
			dfs2(1);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h1, w1);
	for i := 1 to h1 do begin
		for j := 1 to w1 do read(a[i, j]);
		readln;
	end;

	readln(h2, w2);
	for i := 1 to h2 do begin
		for j := 1 to w2 do read(b[i, j]);
		readln;
	end;

	ans := false;
	r[0] := 0;
	c[0] := 0;

	dfs1(1);

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
