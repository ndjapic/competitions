program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 300 * 1000;
var
	n, m, i, j, c, ans: int32;
	a, b, color, top, link: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for c := 1 to n do color[c] := 0;
	for j := 1 to m do top[j] := 0;
	ans := 0;

	for i := 1 to n do begin
		readln(a[i], j, b[i]);

		link[i] := top[j];
		top[j] := i;

		if color[a[i]] = 0 then inc(ans);
		inc(color[a[i]]);
	end;

	for j := 1 to m do begin
		i := top[j];
		while i > 0 do begin
			dec(color[a[i]]);
			if color[a[i]] = 0 then dec(ans);
			if color[b[i]] = 0 then inc(ans);
			inc(color[b[i]]);
			i := link[i];
		end;
		writeln(ans);
	end;
end.
