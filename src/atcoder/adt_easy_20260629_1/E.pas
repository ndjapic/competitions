program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
var
	n, i, l, r, ans: int32;
	s: string;
	ch: char;
	seen: tdictionary<string, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	ans := 0;
	seen := tdictionary<string, boolean>.create;

	for i := 1 to n do begin
		readln(s);
		if not seen.ContainsKey(s) then begin
			inc(ans);
			seen.AddOrSetValue(s, true);

			l := 1;
			r := length(s);
			while l < r do begin
				ch := s[l];
				s[l] := s[r];
				s[r] := ch;
				inc(l);
				dec(r);
			end;

			seen.AddOrSetValue(s, true);
		end;
	end;

	writeln(ans);
	seen.free;
end.
