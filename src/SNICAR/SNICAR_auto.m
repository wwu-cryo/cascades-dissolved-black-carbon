%% SNICAR Automation code
% Anne Martine Wilce, 2023

% This code is intended to be used with The Snow, Ice, and Aerosol 
% Radiative Model with Adding-Doubling solver, version 3.0 (SNICAR-ADv3).
% It calls the snicarAD_v3 code to process multiple iterations of the SNICAR code at a time. 

% Input is to follow a certain format, see template version for more info.
% The template contains a meta-data tab describing each variable, descriptions of 
% which are obtained from the comments in the snicarAD_v3 code itself.

% Multiple tabs can be used to separate by location (or otherwise), and
% multiple samples should be represented as individual columns.

% Blank values can be used to refer to a "default," which will be the sample first
% column (no blanks are allowed in the first sample column).

% Output is given as the spectral albedo at each wavelength, and broadband
% albedo (single value) for each sample in a cumulative tab. 

% If locations contain clean samples as a baseline, 
% this output can then be updated with Global Irradiance data from the 
% solar spectrum calculator found here:
% https://www2.pvlighthouse.com.au/calculators/solar%20spectrum%20calculator/solar%20spectrum%20calculator.aspx
% and run through the automated Radiative Forcing code, RadForce.m.

% Set up instructions are listed below.

% Note on algae optical library:
% If cell_nbr_conc(n) = 0 (cell concentration) the algae library is not required.

%% Path set up

clear %clear workspace

% Use the snicarAD_v3_auto that is edited to pair with this automation

% SNICAR Root path (to the snicar_480band folder)
% is needed for the snicarAD_v3 to run but added as input in this 
% script (add below in Input 1) so that it doesn't
% have to be adjusted in the automated version.

% Add the adjusted snicarAD_v3_auto to your MATLAB path (run this script, and you 
% will be prompted with an error to add it if it is not).


% User defined path below:

% ------------------------------Input 1---------------------------------%

%MAC
%dir_op_root =  '/Volumes/CRYOSHARE/SNICAR/snicar_480band/';

%PC
dir_op_root =  'Z:\SNICAR\snicar_480band';

% ----------------------------------------------------------------------%

%% Input file selection

% You can use either a hard coded path for your input file here (good for testing
% environment or debugging) or comment
% these two lines out and have a GUI pop up. 

% --------------------------Input 2 (optional)--------------------------%

% Manual file read in, ie., adjust the names/paths:
% !! (comment out if using the GUI option) !!

%PC
filepath =  'Z:\SNICAR\Automation';

%MAC
%filepath =  '/Volumes/CRYOSHARE/SNICAR/Automation/';
%file2read = 'template.xlsx'; 

% ----------------------------------------------------------------------%

%GUI file read in, i.e., user selects file in pop up
if  ~exist('file2read','var')
    beep;fprintf('Select the SNICAR input .xslx data file to process:\n')
    pause(2)
    [file2read, filepath]=  uigetfile('*');
end

% Move to the input file location
cd(filepath) 
full_path = [filepath,file2read];

% Read in the data file
[~,sheet_name]=xlsfinfo(full_path);

%% Sheet loop
for sheet_itr=1:numel(sheet_name) %assume Metadata tab is first and skip it- TO DO: make more robust
    input_tab = readtable(full_path,'Sheet',sheet_name{sheet_itr},'VariableNamingRule','preserve');
    fprintf(['Sheet ',sheet_name{sheet_itr}, ' is being processed.\n'])
    headerInfo = input_tab.Properties.VariableNames;

    all_data_out = []; %initialize

    input_tab = table2cell(input_tab); % Convert the table to a cell array

    % Extract the first column values as field names
    fieldNames = input_tab(:, 1);

    %% Column loop
    % Loop through each column/sample
    for col_itr=1:width(input_tab)-1

        %Check for NaN's
        input_args = cell2mat(input_tab(:, col_itr+1));
        nan_inds = find(isnan(input_args));

        if ~isempty(nan_inds)
            beep;
            fprintf(['NaN values in ',headerInfo{col_itr+1},' have been replaced with values from the first sample column.\n'])
            fprintf(['The first sample column is titled ', headerInfo{2}, '. Please ensure missing data and replacement data is intentional.\n'])
            for row_itr=1:length(nan_inds)
                input_tab{nan_inds(row_itr),col_itr+1}= input_tab{nan_inds(row_itr),2};
            end
        end
        %redefine this now as a structure (and w/ replaced data if NaNs
        %were present
        input_args = cell2struct(input_tab(:, col_itr+1), fieldNames, 1);

        % CALL SNICAR WITH INPUT ARGUMENTS
        di = snicarAD_v3_auto(input_args,dir_op_root);

        % plot modeled spectral albedo:
        if (1==0) %toggle on/off
            plot(di.wvl,di.albedo,'b','linewidth',3);
            axis([0.2 2.5 0 1]);
            set(gca,'xtick',0.2:0.2:2.5,'fontsize',14)
            set(gca,'ytick',0:0.1:1.0,'fontsize',14);
            xlabel('Wavelength (\mum)','fontsize',20);
            ylabel('Hemispheric Albedo','fontsize',20);
            grid on;
        end

        %only include wavelength for the first iteration
        % change from um to nm
        if col_itr == 1
            data_out=[di.wvl*1000;di.albedo]';
            data_out = array2table(data_out);
            headerCell={'wavelength';headerInfo{col_itr+1}}';
        else
            data_out=[di.albedo]';
            data_out = array2table(data_out);
            headerCell={headerInfo{col_itr+1}}';
        end
        data_out.Properties.VariableNames = headerCell; %apply header

        %%keep this around if we want to compare the fluxes
        %data_out=[di.wvl;di.albedo;di.flx_dwn_spc;di.abs_spc]';
        %data_out=[di.wvl;di.albedo;di.flx_dwn_spc]';
        %headerCell={'wavelenth';'albedo';'flux'}';
        
        %append each column output info to the prior
        if col_itr == 1
            all_data_out = data_out;
        else
            all_data_out = horzcat(all_data_out, data_out);
        end
        
        %Broadband Snow Albedo tab
        if sheet_itr == 1 && col_itr == 1
            albedo_tab = [sheet_name{sheet_itr}; {headerInfo{col_itr+1}}; di.alb_slr]';
        else
            albedo_tab = vertcat(albedo_tab, [sheet_name{sheet_itr}; {headerInfo{col_itr+1}}; di.alb_slr]');
        end

    end

    %% Write Table
    saveTabName =  [file2read(1:end-5),'_snicar_output.xlsx']; 

    %delete old file for good measure (only first first tab iteration)
    %       (because of the way we are saving tabs)
    if exist(saveTabName, 'file')==2 && sheet_itr==1
        delete(saveTabName)
    end

    writetable(all_data_out,saveTabName,'Sheet', sheet_name{sheet_itr})
    fprintf(['Sheet for ',sheet_name{sheet_itr}, ' has been written.\n'])

end

% Add the Broadband Albedo tab
writecell(albedo_tab,saveTabName,'Sheet', 'BroadbandAlbedo')

fprintf('Broadband Albedo sheet for all locations and samples has been written.\n')
fprintf(['All sheets have been created in file ' saveTabName,'\n'])

%Complete!
