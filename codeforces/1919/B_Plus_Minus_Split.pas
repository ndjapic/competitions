program B_Plus_Minus_Split;
{$H+}
const
    maxn = 5000;
var
    ntc, tci: int16;
    n, i, penalty: int16;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
		readln(s);

        penalty := 0;
        for i := 1 to n do
            case s[i] of
                '+': inc(penalty);
                '-': dec(penalty);
            end;

        writeln(abs(penalty));

    end;
end.
