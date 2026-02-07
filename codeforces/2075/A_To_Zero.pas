program A_To_Zero;
var
    ntc, tci: int16;
    n, k, x: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, k);

        x := k - (n+k) mod 2;
        dec(n, x);
        x := k - k mod 2;

        writeln(1 + (n+x-1) div x);

    end;

end.
