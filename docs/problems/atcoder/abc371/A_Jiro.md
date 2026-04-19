# Problem: A_Jiro.pas

```pascal
program A_Jiro;
var
    s: string;

begin
    readln(s);
    case s[1] of

        '<': begin
            case s[3] of

                '<': begin
                    case s[5] of
                        '<': writeln('B');
                        '>': writeln('C');
                    end;
                end;

                '>': begin
                    case s[5] of
                        '<': writeln('A');
                        '>': writeln('A');
                    end;
                end;

            end;
        end;

        '>': begin
            case s[3] of

                '<': begin
                    case s[5] of
                        '<': writeln('A');
                        '>': writeln('A');
                    end;
                end;

                '>': begin
                    case s[5] of
                        '<': writeln('C');
                        '>': writeln('B');
                    end;
                end;

            end;
        end;

    end;
end.

```
