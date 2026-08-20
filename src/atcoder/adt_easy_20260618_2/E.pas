program C_Striped_Horse;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dictionary #splitstring #strtoint
uses
	generics.collections,
	generics.defaults,
	math, sysutils, strutils;
const
	NN = 100 * 1000;
var
	n, i, best: int32;
	line: string;
	sa: TStringArray;
	s: array [1 .. NN] of string;
	t: array [1 .. NN] of int32;
	seen: tdictionary<string, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	seen := tdictionary<string, boolean>.create;
	best := 0;

	for i := 1 to n do begin

		readln(line);
		sa := SplitString(line, ' '); 
		s[i] := sa[0];
		t[i] := strtoint(sa[1]);

		if not seen.ContainsKey(s[i]) then begin
			if (best = 0) or (t[best] < t[i]) then best := i;
			seen.AddOrSetValue(s[i], True);
		end;

	end;

	writeln(best);
	seen.free;
end.
