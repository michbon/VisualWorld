% Add BSI and other variables to Binomial Responses data

clear all; clc

% % linguistic variables from the questionnaires
variables   = readtable('all_variables_Ita-Eng.csv', 'ReadVariableNames', true);
vsubject    = variables.Properties.VariableNames;

% % the dataset you want to merge
data = 'fixations';

if strcmp(data, 'responses')
  bino = readtable('binomial_responses_IE.csv');
else
  bino = readtable('BinomialFixations_IE.csv'); 
end

bsubject    = table2cell(bino(:, 'subject'));


for b = 1:length(bsubject)
    
    for v = 1: length(vsubject)
        if strcmp(bsubject(b), vsubject(v))
            
            %% age
            age(b, 1) = cellfun(@str2num, table2array(variables(5, v)));
            
            %% years of education
            edu(b, 1) = cellfun(@str2num, table2array(variables(8, v)));
            
            %% bsi
            bsi(b, 1) = table2array(variables(4, v));
            
            %% proficiency
            profL1(b, 1) = mean(cellfun(@str2num,...
                table2array(variables(11:14, v))));
            profL2(b, 1)= mean(cellfun(@str2num,...
                table2array(variables(25:28, v))));
            
            %% age of acquisition
            
            %   L1
            %   when did you begin learning L1?
            L1_acq_learn(b, 1)= cellfun(@str2num,...
                table2array(variables(9, v)));
            
            if L1_acq_learn(b, 1) < 6
                L1_acq_learn_num(b, 1) = 1;
                
            elseif L1_acq_learn(b, 1) >= 6 && L1_acq_learn(b, 1) < 12
                L1_acq_learn_num(b, 1) = 2;
                
            elseif L1_acq_learn(b, 1) >= 12 && L1_acq_learn(b, 1) < 18
                L1_acq_learn_num(b, 1) = 3;
                
            elseif L1_acq_learn(b, 1) >= 18
                L1_acq_learn_num(b, 1) = 4;
                
            else L1_acq_learn_num(b, 1) = 5;
            end
            
            %   when did you begin speaking L1?
            L1_acq_speak(b, 1)= cellfun(@str2num,...
                table2array(variables(10, v)));
            if L1_acq_speak(b, 1) < 6
                L1_acq_speak_num(b, 1) = 1;
                
            elseif L1_acq_speak(b, 1) >= 6 && L1_acq_speak(b, 1) < 12
                L1_acq_speak_num(b, 1) = 2;
                
            elseif L1_acq_speak(b, 1) >= 12 && L1_acq_speak(b, 1) < 18
                L1_acq_speak_num(b, 1) = 3;
                
            elseif L1_acq_speak(b, 1) >= 18
                L1_acq_speak_num(b, 1) = 4;
                
            else L1_acq_speak_num(b, 1) = 5;
            end
            
            %   pool them in categories
            if L1_acq_learn_num(b, 1) == L1_acq_speak_num(b, 1)
                L1_acq_all_num(b, 1) = L1_acq_learn_num(b, 1);
            else
                L1_acq_all_num(b, 1) = max(L1_acq_learn_num(b, 1), L1_acq_speak_num(b, 1));
            end
            
            % from number to readable labels
            if L1_acq_all_num(b, 1) == 1
                L1_acq_all_cat(b, 1) = {'early'};
            elseif L1_acq_all_num(b, 1) == 2
                L1_acq_all_cat(b, 1) = {'child'};
            elseif L1_acq_all_num(b, 1) == 3
                L1_acq_all_cat(b, 1) = {'teen'};
            elseif L1_acq_all_num(b, 1) == 4
                L1_acq_all_cat(b, 1) = {'adult'};
            elseif L1_acq_all_num(b, 1) == 5
                L1_acq_all_cat(b, 1) = {'never'};
            end
            
            %   L2
            %   when did you begin learning L2?
            L2_acq_learn(b, 1)= cellfun(@str2num,...
                table2array(variables(22, v)));
            if L2_acq_learn(b, 1) < 6
                L2_acq_learn_num(b, 1) = 1;
                
            elseif L2_acq_learn(b, 1) >= 6 && L2_acq_learn(b, 1) < 12
                L2_acq_learn_num(b, 1) = 2;
                
            elseif L2_acq_learn(b, 1) >= 12 && L2_acq_learn(b, 1) < 18
                L2_acq_learn_num(b, 1) = 3;
                
            elseif L2_acq_learn(b, 1) >= 18
                L2_acq_learn_num(b, 1) = 4;
                
            else L2_acq_learn_num(b, 1) = 5;
            end
            
            %   when did you begin speaking L2?
            L2_acq_speak(b, 1)= cellfun(@str2num,...
                table2array(variables(23, v)));
            if L2_acq_speak(b, 1) < 6
                L2_acq_speak_num(b, 1) = 1;
                
            elseif L2_acq_speak(b, 1) >= 6 && L2_acq_speak(b, 1) < 12
                L2_acq_speak_num(b, 1) = 2;
                
            elseif L2_acq_speak(b, 1) >= 12 && L2_acq_speak(b, 1) < 18
                L2_acq_speak_num(b, 1) = 3;
                
            elseif L2_acq_speak(b, 1) >= 18
                L2_acq_speak_num(b, 1) = 4;
                
            else L2_acq_speak_num(b, 1) = 5;
            end
            
            %   pool them in categories
            if L2_acq_learn_num(b, 1) == L2_acq_speak_num(b, 1)
                L2_acq_all_num(b, 1) = L2_acq_learn_num(b, 1);
            else
                L2_acq_all_num(b, 1) = max(L2_acq_learn_num(b, 1), L2_acq_speak_num(b, 1));
            end
            
            % from number to readable labels
            if L2_acq_all_num(b, 1) == 1
                L2_acq_all_cat(b, 1) = {'early'};
            elseif L2_acq_all_num(b, 1) == 2
                L2_acq_all_cat(b, 1) = {'child'};
            elseif L2_acq_all_num(b, 1) == 3
                L2_acq_all_cat(b, 1) = {'teen'};
            elseif L2_acq_all_num(b, 1) == 4
                L2_acq_all_cat(b, 1) = {'adult'};
            elseif L2_acq_all_num(b, 1) == 5
                L2_acq_all_cat(b, 1) = {'never'};
            end
            
            %% exposure
            expL1(b, 1)= mean(cellfun(@str2num,...
                table2array(variables(17:21, v))));
            expL2(b, 1)= mean(cellfun(@str2num,...
                table2array(variables(31:35, v))));
            
            %% n contexts
            ncontextL1(b, 1) = cellfun(@str2num, table2array(variables(15, v)));
            ncontextL2(b, 1) = cellfun(@str2num, table2array(variables(29, v)));
            
            %% frequency of switching
            freqsw(b, 1) = cellfun(@str2num, table2array(variables(36, v)));
            
            %% "since when are you multilingual?" categorical
            multi(b, 1)= table2cell(variables(44, v));
            
            
        end
    end
    
end

tvar = [bino...
    array2table(age)...
    array2table(edu)...
    bsi...
    array2table(profL1)...
    array2table(profL2)...
    array2table(L1_acq_all_cat)...
    array2table(L2_acq_all_cat)...
    array2table(expL1)...
    array2table(expL2)...
    array2table(ncontextL1)...
    array2table(ncontextL2)...
    array2table(freqsw)...
    multi...
    ];
tvar.Properties.VariableNames = ...
    strrep(tvar.Properties.VariableNames, 'Var10', 'bsi'); 
tvar.Properties.VariableNames(end) = {'multi'};
writetable(tvar, ['binomial_' data '_var_IE.csv'])

