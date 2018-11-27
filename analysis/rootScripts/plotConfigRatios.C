#include <stdlib.h>
#include "Riostream.h"
#include "TGraphErrors.h"
#include <stdio.h>
#include "TROOT.h"
#include <iostream>
#include <vector>
#include <sstream>
#include <string>
using namespace std;

vector<vector<string>> CSVParse(TString fileName)
{
	//in.open(Form("%sbasic.dat",fileName.Data()));
	//ifstream in("Lab8Data.csv");
	ifstream in(fileName.Data());
	string line, field1, field2, field3, field4, field5;

	vector<vector<string> > array;  // the 2D array
	vector<string> v;                // array of values for one line only

	//getline(in,line); //skip header line

	while ( getline(in,line) )    // get next line in file
	{
		v.clear();
		stringstream ss(line);

		getline(ss,field1,',');  // break line into comma delimitted fields
		getline(ss,field2,',');
		getline(ss,field3,',');
		getline(ss,field4,',');
		ss << "\n";
		getline(ss,field5,'\n');
		//field5 = ss.str();

		//field1 = atof(field.c_str());
		v.push_back(field1);  // add each field to the 1D array
		v.push_back(field2);
		v.push_back(field3);
		v.push_back(field4);
		v.push_back(field5);

		array.push_back(v);  // add the 1D array to the 2D array
	}
	in.close();
	return array;
}

void plotConfigRatios(string fileName = "NEIL_1001") {
	string mode = "Modified"; // "Benchmark";//"Modified"

	// Format: thickness,mm,5,0.99041,0.0301189
	vector<vector<string>> data = CSVParse(Form("output/SAM_analysis_%s.csv",fileName.c_str()));
	int len = data.size();
	// Plot data
	//   for each entry in the vector, open the sub-vector and print the modifier to the plot name (with the units)
	//     if the 2nd and 3rd entries in the vector are "benchmark" then only conver the last two entries to doubles, and plot these as special points in additional plots to be merged in
	//     plot the rest of the entries and error vs the number parameter
	//   done

	std::cout<<"length of vector = "<<len<<std::endl;
	string *modifiers = new string[len];
	string *units = new string[len];
	double *number = new double[len];
	double *value = new double[len];
	double *error = new double[len];
	int n_benchmarks = 0;
	for (int i=0; i<len; i++){
		if (data[i][1]=="benchmark" || data[i][2]=="benchmark"){
			number[i]=n_benchmarks; //modified position value doesn't make sense
			n_benchmarks++;
		}
		else {
			std::stringstream iss3(data[i][2]);
			iss3 >> number[i];
		}
		modifiers[i]=data[i][0];
		units[i]=data[i][1];
		std::stringstream iss4(data[i][3]);
		iss4 >> value[i];	
		std::stringstream iss5(data[i][4]);
		iss5 >> error[i];
	}
	int nBen = n_benchmarks;	
	string *newModifiers = new string[len-nBen];
	string *newUnits = new string[len-nBen];
	double *newNumber = new double[len-nBen];
	double *newValue = new double[len-nBen];
	double *newError = new double[len-nBen];
	string *modifiersBen = new string[nBen];
	double *numberBen = new double[nBen];
	double *valueBen = new double[nBen];
	double *errorBen = new double[nBen];
	

	TCanvas * c1 = new TCanvas();
	for (int j=0;j<len;j++){
		printf("%d,%f,%f,%f \n",len,number[j],value[j],error[j]);
	}
	
	std::copy(modifiers + nBen, modifiers + len, newModifiers);
	std::copy(units + nBen, units + len, newUnits);
	std::copy(number + nBen, number + len, newNumber);
	std::copy(value + nBen, value + len, newValue);
	std::copy(error + nBen, error + len, newError);

	std::copy(modifiers, modifiers + nBen, modifiersBen);
	std::copy(number, number + nBen, numberBen);
	std::copy(value, value + nBen, valueBen);
	std::copy(error, error + nBen, errorBen);

	vector<double> sortedNumber(newNumber, newNumber + len - nBen);
	std::sort(sortedNumber.begin(),sortedNumber.end());
	double numberMin = sortedNumber[0];
	double numberMax = sortedNumber[len-1-nBen];

	if (mode=="Benchmark"){
		TGraphErrors * graphBen = new TGraphErrors(nBen,numberBen,valueBen,0,errorBen);
		graphBen->SetTitle(Form("Plot of %s %s configurations",fileName.c_str(),newModifiers[0].c_str()));
		graphBen->SetMarkerColor(4);
		graphBen->SetMarkerStyle(21);
		graphBen->GetXaxis()->SetTitle(Form("Benchmarks"));
		graphBen->GetYaxis()->SetTitle(Form("Relative %s",fileName.c_str()));
		//graph->SetMarkerSize(.4);
		graphBen->Draw("APE");
		c1->Update();
		c1->SaveAs(Form("Plot_%s_%s.pdf",modifiersBen[0].c_str(),fileName.c_str()));
	}
	else{
		TGraphErrors * graph = new TGraphErrors(len-nBen,newNumber,newValue,0,newError);
		graph->SetTitle(Form("Plot of %s %s configurations, size %.1f to %.1f %s",fileName.c_str(),newModifiers[0].c_str(),numberMin,numberMax,newUnits[0].c_str()));
		graph->SetMarkerColor(4);
		graph->SetMarkerStyle(21);
		graph->Fit("pol1");
		gStyle->SetOptFit(0111);
		graph->GetXaxis()->SetTitle(Form("%s %s",newModifiers[0].c_str(),newUnits[0].c_str()));
		graph->GetYaxis()->SetTitle(Form("Relative %s",fileName.c_str()));
		//graph->SetMarkerSize(.4);
		graph->Draw("APE");
		c1->Update();
		c1->SaveAs(Form("Plot_%s_%s.pdf",modifiers[4].c_str(),fileName.c_str()));
	}
	delete[] modifiers;
	delete[] units;
	delete[] number;
	delete[] value;
	delete[] error;
	delete[] modifiersBen;
	delete[] valueBen;
	delete[] errorBen;
}
