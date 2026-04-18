program C_Make_it_Equal;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
var
	ntc, tci: int16;
	n, i, k, x: int32;
	s, t: TList<int32>;

begin
	randomize;
	s := TList<int32>.Create;
	t := TList<int32>.Create;

	try

		readln(ntc);
		for tci := 1 to ntc do begin

			readln(n, k);

			s.Clear;
			for i := 0 to n-1 do begin
				read(x);
				x := x mod k;
				s.Add(min(x, k-x));
				s.Exchange(i, random(i+1));
			end;
			readln;
			s.Sort;

			t.Clear;
			for i := 0 to n-1 do begin
				read(x);
				x := x mod k;
				t.Add(min(x, k-x));
				t.Exchange(i, random(i+1));
			end;
			readln;
			t.Sort;

			i := 0;
			while (i < n) and (s[i] = t[i]) do inc(i);

			if i < n then
				writeln('NO')
			else
				writeln('YES');

		end;

	finally
		s.Free;
		t.Free;
	end;
end.
