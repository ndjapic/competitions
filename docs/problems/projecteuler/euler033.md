# Problem: euler033.pas

```pascal
program euler033;
uses
	math;
var
	n, k, e, i, j, mnom, mden: int8;
	nom1, den1, nom2, den2: int32;
	snom, sden: int32;
	found, loop: boolean;
	digits, p: array [0 .. 3] of int8;
	seen: array [0 .. 3] of boolean;
	pow10: array [0 .. 4] of int16;

function c1(m: int8): int8;
begin
	if m < 2 then
		c1 := m
	else
		c1 := c1(m div 2) + m mod 2;
end;

begin
	readln(n, k);
	snom := 0;
	sden := 0;

	pow10[0] := 1;
	for e := 1 to n do
		pow10[e] := 10 * pow10[e-1];

	for mnom := 1 to (1 shl n) - 2 do if c1(mnom) = k then
	for nom1 := pow10[n-1] to pow10[n] - 1 do begin

		nom2 := 0;
		i := k;
		found := false;
		for e := n-1 downto 0 do
			if odd(mnom shr e) then begin
				dec(i);
				digits[i] := nom1 div pow10[e] mod 10;
				if digits[i] = 0 then found := true;
			end else
				nom2 := 10 * nom2 + nom1 div pow10[e] mod 10;

		if not found then
			for mden := 1 to (1 shl n) - 2 do if c1(mden) = k then
			for den2 := pow10[n-k-1] to pow10[n-k] - 1 do begin

				for i := 0 to k-1 do begin
					p[i] := i;
					seen[i] := true;
				end;

				loop := true;
				while loop do begin

					den1 := 0;
					i := k;
					j := n-k;
					for e := n-1 downto 0 do
						if odd(mden shr e) then begin
							dec(i);
							den1 := 10 * den1 + digits[p[i]];
						end else begin
							dec(j);
							den1 := 10 * den1 + den2 div pow10[j] mod 10;
						end;

					if (nom1 < den1) and (den1 >= pow10[n-1]) {and (
						(nom1 mod 10 > 0) or (den1 mod 10 > 0)
					)} and (nom1 * den2 = nom2 * den1) then begin
						inc(snom, nom1);
						inc(sden, den1);
						{writeln(nom1, '/', den1, ' = ', nom2, '/', den2);}
					end;

					seen[p[k-1]] := false;
					i := k-2;
					while (i >= 0) and (p[i] > p[i+1]) do begin
						seen[p[i]] := false;
						dec(i);
					end;

					loop := i >= 0;
					if loop then begin

						p[i+1] := min(p[i], p[k-1]);
						seen[p[i]] := false;
						inc(p[i]);
						while seen[p[i]] do inc(p[i]);
						seen[p[i]] := true;
						inc(i);

						while i < k do begin
							while seen[p[i]] do inc(p[i]);
							seen[p[i]] := true;
							inc(i);
							if i < k then
								p[i] := p[i-1];
						end;

					end;

				end;

			end;

	end;

	writeln(snom, ' ', sden);
end.

```
