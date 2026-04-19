# Problem: D_Substr_Swap.pas

```pascal
program D_Substr_Swap;
{$MODE DELPHI}
const
	nn = 500 * 1000;
var
	n, m, i, l, r: int32;
	s, t: string;
	isSwapped: array [0 .. nn] of boolean;

begin
	readln(n, m);
	readln(s);
	readln(t);

	for i := 0 to n do isSwapped[i] := false;

	for i := 1 to m do begin
		readln(l, r);
		isSwapped[r] := not isSwapped[r];
		isSwapped[l-1] := not isSwapped[l-1];
	end;

	for i := n downto 1 do begin
		if isSwapped[i] then s[i] := t[i];
		isSwapped[i-1] := isSwapped[i-1] xor isSwapped[i];
	end;

	writeln(s);
end.

```
