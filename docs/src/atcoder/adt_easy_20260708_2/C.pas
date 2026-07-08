program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, ii, ans: int8;
	d: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(d[i]);
	readln;

	ans := 0;
	for i := 1 to 9 do begin
		ii := 11 * i;

		if i <= n then begin
			if i <= d[i] then inc(ans);
			if ii <= d[i] then inc(ans);
		end;

		if ii <= n then begin
			if i <= d[ii] then inc(ans);
			if ii <= d[ii] then inc(ans);
		end;
	end;

	writeln(ans);
end.
