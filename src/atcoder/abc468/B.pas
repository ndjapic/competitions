program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	MM = 100;
var
	m, d, i, l, r, ans: int32;
	s: string;
	c: array [0 .. MM] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(m, d);
	readln(s);

	for i := 0 to m do c[i] := 0;

	for i := 1 to m do
		if s[i] = 'G' then begin
			l := i-d;
			r := min(i+d, m);

			if l > 0 then dec(c[l-1]);
			inc(c[r]);
		end;

	ans := 0;
	for i := m downto 1 do begin
		if c[i] = 0 then inc(ans);
		inc(c[i-1], c[i]);
	end;

	writeln(ans);
end.
