program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #hashset
uses
	generics.collections;
var
	n, i, a: int32;
	k, ans: int64;
	seen: tdictionary<int32, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	seen := tdictionary<int32, boolean>.create;
	ans := k * (k+1) div 2;

	for i := 1 to n do begin
		read(a);
		if (a <= k) and not seen.ContainsKey(a) then begin
			dec(ans, a);
			seen.addorsetvalue(a, true);
		end;
	end;
	readln;

	writeln(ans);
	seen.free;
end.
