program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, f, p, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	p := n;

	for i := 1 to n do begin
		readln(f);
		p := max(0, p - f);

		if p > 0 then begin
			dec(p);
			inc(ans);
		end;
	end;

	writeln(ans);
end.
