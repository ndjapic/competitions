program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	i := 1;
	ans := 0;
	while i <= n do
		if (s[i] = '0') and (i < n) and (s[i+1] = '0') then begin
			inc(ans);
			inc(i, 2);
		end else begin
			inc(ans);
			inc(i);
		end;

	writeln(ans);
end.
