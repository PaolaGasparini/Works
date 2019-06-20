function [emg_n,act]= timing_emg_cycling_new(emg,fc_emg,bias,media,var,Tempo)

%passa alto a 10 Hz
ft_h = 10;
Wn_h= ft_h/(fc_emg/2);
[b_h,a_h] = butter(5,Wn_h,'high');

%passa basso a 6 Hz
ft_p = 6;
Wn_p= ft_p/(fc_emg/2);
[b_p,a_p] = butter(2,Wn_p);

n_canali=min(size(emg));

act=zeros(length(emg),n_canali);

for i=1:n_canali
    emg_i=emg(:,i);
    emg_h=filtfilt(b_h,a_h,emg_i);
    emg_r=abs(emg_h);
    emg_p= filtfilt(b_p,a_p,emg_r);
    index=find(emg_p > media(i) + var(i)*bias);
    act(index,i)=1;
    index_salti=find(diff(act(:,i))~=0);
    for n=2:2:length(index_salti)
        if (index_salti(n)-index_salti(n-1)>20)
            act((index_salti(n-1):index_salti(n)),i)=1;
        else
            act((index_salti(n-1):index_salti(n)),i)=0;
        end
    end
    act((index_salti(n):end),i)=0;
    emg_t(:,i)=emg_p;
    clear index
    clear index_salti
end

emg_t=emg_t.*act;
M=max(emg_t(12000:end,:)); 


 risp=menu('Normalizzazione rispetto a:','Massimo ottenuto durante la pedalata','MVC');
 if risp==1
%normalizzo rispetto al massimo di attivazione ottenuta nella fase
%attiva..trascuro il passivo anche se spesso si ha un'attivazione 
for n=1:length(M)
    if M(n)==0
        emg_n(:,n)=zeros(max(size(emg_t)),1); 
    else 
        emg_n(:,n)=emg_t(:,n)/M(n);      
    end
end
      
 else
 if risp==2 %%DA FARE
     [M, emg]=MVC_EMG(fc_emg,Tempo)%M è vettore righe con tutti i massimi
    for n=1:length(M)
      if M(n)==0
        emg_n(:,n)=zeros(max(size(emg_t)),1); 
      else 
        emg_n(:,n)=emg_t(:,n)/M(n);
      end
    end         
 end
 end
 

