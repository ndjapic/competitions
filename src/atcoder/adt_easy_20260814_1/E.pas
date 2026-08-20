program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(s);
	n := length(s);

	ans := 0;
	i := 1;
	while i <= n do
		if s[i] > '0' then begin
			inc(ans);
			inc(i);
		end else if i = n then begin
			inc(ans);
			inc(i);
		end else if s[i+1] > '0' then begin
			inc(ans);
			inc(i);
		end else begin
			inc(ans);
			inc(i, 2);
		end;

	writeln(ans);
end.
