# Problem: C_Prefix_Min_and_Suffix_Max.pas

```pascal
program C_Prefix_Min_and_Suffix_Max;
{$MODE DELPHI}
uses
	math;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i: int32;
	s: string;
	a, pre, suf: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		setlength(s, n);

		for i := 1 to n do begin
			read(a[i]);
		end;
		readln;

		pre[1] := a[1];
		for i := 2 to n do pre[i] := min(pre[i-1], a[i]);

		suf[n] := a[n];
		for i := n-1 downto 1 do suf[i] := max(suf[i+1], a[i]);

		for i := 1 to n do
			if a[i] = pre[i] then
				s[i] := '1'
			else if a[i] = suf[i] then
				s[i] := '1'
			else
				s[i] := '0';

		writeln(s);

	end;
end.

```
