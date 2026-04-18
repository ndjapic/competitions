program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, m, i, ans: int32;
	s: string;
	ch: char;
	c: array ['1' .. '2'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	readln(s);

	for ch := '1' to '2' do c[ch] := 0;
	ans := 0;

	for i := 1 to n do begin
		ch := s[i];
		if ch = '0' then begin
			for ch := '1' to '2' do c[ch] := 0;
		end else begin
			inc(c[ch]);
			ans := max(ans, max(0, c['1'] - m) + c['2']);
		end;
	end;

	writeln(ans);
end.
