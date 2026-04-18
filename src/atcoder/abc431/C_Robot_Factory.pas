program C_Robot_Factory;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections;
const
	nn = 200 * 1000;
var
	n, m, k, c, i, j, x: int32;
	h, b: TList<int32>;

begin
	randomize;
	h := TList<int32>.Create;
	b := TList<int32>.Create;
	try

		readln(n, m, k);

		for i := 0 to n-1 do begin
			read(x);
			h.Add(x);
			h.Exchange(i, random(i+1));
		end;
		readln;
		h.Sort;

		for j := 0 to m-1 do begin
			read(x);
			b.Add(x);
			b.Exchange(j, random(j+1));
		end;
		readln;
		b.Sort;

		c := 0;
		j := 0;
		for i := 0 to n-1 do begin
			while (j < m) and (b[j] < h[i]) do inc(j);
			if j < m then begin
				inc(c);
				inc(j);
			end;
		end;

		if c >= k then
			writeln('Yes')
		else
			writeln('No');

	finally
		h.Free;
		b.Free;
	end;
end.
