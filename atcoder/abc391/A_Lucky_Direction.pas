program A_Lucky_Direction;
{$mode delphi}{$inline on}
var
    i: int8;
    d: string;

begin
    readln(d);

    for i := 1 to length(d) do
        case d[i] of
            'N': d[i] := 'S';
            'E': d[i] := 'W';
            'W': d[i] := 'E';
            'S': d[i] := 'N';
        end;

    writeln(d);
end.
