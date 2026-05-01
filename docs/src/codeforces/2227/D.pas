program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100 * 1000;
var
	notc, tci, n, i, ans: int32;
	a: array [1 .. 2*nn] of int32;
	le, ri: array [0 .. nn] of int32;
	seen: array [0 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function mex(l, r: int32): int32;
var
	i: int32;
begin
	for i := 0 to n do seen[i] := false;
	result := 0;

	i := 0;
	while (l+i <= r-i) and (a[l+i] = a[r-i]) do begin
		seen[a[l+i]] := true;
		inc(i);
	end;

	if l+i > r-i then begin
		i := 0;
		while (l-i >= 1) and (r+i <= 2*n) and (a[l-i] = a[r+i]) do begin
			seen[a[l-i]] := true;
			inc(i);
		end;
		while seen[result] do inc(result);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to 2*n do read(a[i]);
		readln;

		for i := 1 to 2*n do ri[a[i]] := i;
		for i := 2*n downto 1 do le[a[i]] := i;

		ans := mex(le[0], ri[0]);
		ans := max(ans, mex(le[0], le[0]));
		ans := max(ans, mex(ri[0], ri[0]));

		writeln(ans);

	end;
end.
