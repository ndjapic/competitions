program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort
uses
	Generics.Collections,
	Generics.Defaults;
const
	NN = 200 * 1000;
var
	ntc, tci, n, i, l, r: int32;
	ans: int8;
	s: array [1 .. NN] of int32;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(s[i]);
		readln;

		if s[n] <= 2 * s[1] then
			ans := 2
		else begin

			a := TList<int32>.Create;
			for i := 1 to n do
				if (s[1] <= s[i]) and (s[i] <= s[n]) then begin
					a.Add(s[i]);
					a.Exchange(a.Count - 1, Random(a.Count));
				end;
			n := a.Count;
			a.Sort;

			ans := 1;
			l := 0;
			for r := 0 to n-1 do
				if (ans > -1) and ((r = n-1) or (a[r+1] > 2 * a[l])) then begin
					if (l < r) and (a[r] <= 2 * a[l]) then begin
						l := r;
						inc(ans);
					end else
						ans := -1;
				end;

			a.Free;

		end;

		writeln(ans);

	end;
end.
