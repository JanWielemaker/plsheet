http://www.j-paine.org/compiling_spreadsheets_to_prolog.html
http://www.j-paine.org/spreadsheet_structure_discovery.html
http://logic.stanford.edu/~mkassoff/papers/introtologicalspreadsheets.pdf
http://arxiv.org/pdf/0803.0163

unoconv -f ods *.xlsx

localc for PMT():
ScGetRmz(nZins, nZzr, nBw, nZw, nFlag)

double ScInterpreter::ScGetRmz(double fZins, double fZzr, double fBw,
                       double fZw, double fF)
{
    double fRmz;
    if (fZins == 0.0)
        fRmz = (fBw + fZw) / fZzr;
    else
    {
        double fTerm = pow(1.0 + fZins, fZzr);
        if (fF > 0.0)
            fRmz = (fZw * fZins / (fTerm - 1.0)
                        + fBw * fZins / (1.0 - 1.0 / fTerm)) / (1.0+fZins);
        else
            fRmz = fZw * fZins / (fTerm - 1.0)
                        + fBw * fZins / (1.0 - 1.0 / fTerm);
    }
    return -fRmz;
}

http://cgit.freedesktop.org/libreoffice/core/tree/sc/source/core/tool/interpr2.cxx
