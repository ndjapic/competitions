program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, ans: int64;
	k: int8;
	digits: array [1 .. 60] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	dec(n);
	k := 0;

	while n > 0 do begin
		inc(k);
		digits[k] := n mod 5;
		n := n div 5;
	end;

	ans := 0;
	while k > 0 do begin
		ans := ans * 10 + digits[k] * 2;
		dec(k);
	end;
	writeln(ans);
end.
