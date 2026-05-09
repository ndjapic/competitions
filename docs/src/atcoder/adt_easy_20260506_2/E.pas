program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	d: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);
	ans := n;

	for i := 2 to n do begin
		d := ord(s[i-1]) - ord(s[i]);
		if d < 0 then inc(d, 10);
		inc(ans, d);
	end;

	writeln(ans + ord(s[n]) - ord('0'));
end.
