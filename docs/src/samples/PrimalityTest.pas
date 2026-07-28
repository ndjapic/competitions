program PrimalityTest;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #Miller #Rabin #primality #test
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	TT = 5000;
var
	t, i: int32;
	m: int64;
	n, ans: array [1 .. TT] of int64;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref l, r: int32): int32;
begin
	Result := CompareValue(n[l], n[r]);
end;

procedure modinc(var a: int64; b, m: int64);
begin
	inc(a, b);
	if a >= m then dec(a, m);
end;

function modmul(a, b, m: int64): int64;
var
	res: int64;
begin
	res := 0;
	a := a mod m;
	while b > 0 do begin
		if odd(b) then modinc(res, a, m);
		modinc(a, a, m);
		b := b div 2;
	end;
	Result := res;
end;

function modpow(base, exp, m: int64): int64;
var
	res: int64;
begin
	res := 1;
	base := base mod m;
	while exp > 0 do begin
		if odd(exp) then res := modmul(res, base, m);
		base := modmul(base, base, m);
		exp := exp div 2;
	end;
	Result := res;
end;

function perfect(m: int64): boolean;
const
	bases: array [0..3] of int64 = (2, 13, 23, 1662803);
var
	d, x, cur_d: int64;
	i: integer;
	is_prime, base_passed: boolean;
begin
	is_prime := (m = 2) or (m > 2) and odd(m);

	if is_prime and (m > 7) then
		is_prime := (m mod 3 > 0) and (m mod 5 > 0) and (m mod 7 > 0);

	if is_prime then begin
		d := m - 1;
		while not odd(d) do d := d div 2;

		i := 0;
		while (i <= 3) and (bases[i] < m) and is_prime do begin
			x := modpow(bases[i], d, m);
			base_passed := (x = 1) or (x = m - 1);

			if not base_passed then begin
				cur_d := d;
				while (cur_d <> m - 1) and not base_passed do begin
					x := modmul(x, x, m);
					cur_d := cur_d * 2;
					
					if x = 1 then
						cur_d := m - 1
					else
						base_passed := x = m - 1;
				end;
			end;

			is_prime := base_passed;
			inc(i);
		end;
	end;

	Result := is_prime;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(t);

	p := TList<int32>.Create;
	for i := 1 to t do begin
		readln(n[i]);
		p.Add(i);
		p.Exchange(i-1, Random(i));
	end;
	p.Sort(TComparer<int32>.Construct(cmp));

	m := 2;
	for i in p do begin
		if m < n[i] then begin
			m := n[i];
			if (m > 2) and not odd(m) then inc(m);

			while not perfect(m) do
				if m = 2 then 
					inc(m)
				else
					inc(m, 2);
		end;
		ans[i] := m - n[i];
	end;

	for i := 1 to t do writeln(ans[i]);

	p.Free;
end.

