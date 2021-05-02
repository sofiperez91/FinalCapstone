library(shiny)
library(plotly)
library(ggplot2)


# Define server logic required to draw a histogram

shinyServer(function(input, output) {
    bi_freq<-readRDS("./bi_freq.RDS")
    tri_freq<-readRDS("./tri_freq.RDS")
    qua_freq<-readRDS("./qua_freq.RDS")
    five_freq<-readRDS("./five_freq.RDS")
    six_freq<-readRDS("./six_freq.RDS")
    stats <-readRDS("./ngram_stats.RDS")

   
    predict_word <- function(input,top) {
        input<-gsub("'","",input)
        input<-gsub('[[:punct:] ]+',' ',input)
        input<-tolower(input)
        input=strsplit(input, " ")[[1]]
        N=length(input)
        word=c()
        freq=c()
        count=c()
        phrase<-c()
        Ngram<-c()
        hits=0
        
        if (N>=5) {
            i=1
            while(hits<top && dim(six_freq)[1]>=i) {
                aux=strsplit(six_freq[i,1], " ")[[1]]
                if (aux[1]==input[N-4] && aux[2]==input[N-3] && aux[3]==input[N-2] && aux[4]==input[N-1] && aux[5]==input[N]){
                    word[hits+1]=aux[6]
                    freq[hits+1]=six_freq[i,2]/dim(six_freq)[1]
                    count[hits+1]=six_freq[i,2]
                    phrase[hits+1]<-six_freq[i,1]
                    Ngram[hits+1]<-"sixgram"
                    hits=hits+1
                } 
                i=i+1
            }
            if (length(word)<top) {
                input[1]=input[N-3]
                input[2]=input[N-2]
                input[3]=input[N-1]
                input[4]=input[N]
                N=4
            }
        }
        
        if (N==4) {
            i=1
            while(hits<top && dim(five_freq)[1]>=i) {
                aux=strsplit(five_freq[i,1], " ")[[1]]
                if (aux[1]==input[N-3] && aux[2]==input[N-2] && aux[3]==input[N-1] && aux[4]==input[N] && !(aux[5] %in% word)){
                    word[hits+1]=aux[5]
                    freq[hits+1]=five_freq[i,2]/dim(five_freq)[1]
                    count[hits+1]=five_freq[i,2]
                    phrase[hits+1]<-five_freq[i,1]
                    Ngram[hits+1]<-"fivegram"
                    hits=hits+1
                } 
                i=i+1
            }
            if (length(word)<top) {
                input[1]=input[N-2]
                input[2]=input[N-1]
                input[3]=input[N]
                N=3
            }
        }
        
        if (N==3) {
            i=1
            while(hits<top && dim(qua_freq)[1]>=i) {
                aux=strsplit(qua_freq[i,1], " ")[[1]]
                if (aux[1]==input[N-2] && aux[2]==input[N-1] && aux[3]==input[N] && !(aux[4] %in% word)){
                    word[hits+1]=aux[4]
                    freq[hits+1]=qua_freq[i,2]/dim(qua_freq)[1]
                    count[hits+1]=qua_freq[i,2]
                    phrase[hits+1]<-qua_freq[i,1]
                    Ngram[hits+1]<-"quagram"
                    hits=hits+1
                } 
                i=i+1
            }
            if (length(word)<top) {
                input[1]=input[N-1]
                input[2]=input[N]
                N=2
            }
            
        }
        
        if (N==2) {
            i=1
            while(hits<top && dim(tri_freq)[1]>=i) {
                aux=strsplit(tri_freq[i,1], " ")[[1]]
                if (aux[1]==input[N-1] && aux[2]==input[N] && !(aux[3] %in% word)){
                    word[hits+1]=aux[3]
                    freq[hits+1]=tri_freq[i,2]/dim(tri_freq)[1]
                    count[hits+1]=tri_freq[i,2]
                    phrase[hits+1]<-tri_freq[i,1]
                    Ngram[hits+1]<-"trigram"
                    hits=hits+1
                } 
                i=i+1
            }
            if (length(word)<top) {
                input[1]=input[N]
                N=1
            }
            
        }
        
        
        if(N==1) {
            i=1
            while(hits<top && dim(bi_freq)[1]>=i) {
                aux=strsplit(bi_freq[i,1], " ")[[1]]
                if (aux[1]==input[N]&& !(aux[2] %in% word)) {
                    word[hits+1]=aux[2]
                    freq[hits+1]=bi_freq[i,2]/dim(bi_freq)[1]
                    count[hits+1]=bi_freq[i,2]
                    phrase[hits+1]<-bi_freq[i,1]
                    Ngram[hits+1]<-"bigram"
                    hits=hits+1
                }
                i=i+1
            }
        } 
        
        pred<-cbind(word,freq,count,phrase,Ngram)
        pred<-data.frame(pred)
        pred$freq<-as.numeric(pred$freq)
        pred$Ngram <- factor(pred$Ngram, levels = c("sixgram", "fivegram", "quagram", "trigram", "bigram"))
        pred
    }    
    
    
    data<-reactive({
    input$data
    })
    
    top<-reactive({
       as.numeric(input$top)
    })
    
    output$next_word <- renderPrint({
        pred<-predict_word(data(),top())
        cat( 
            paste(pred$word, collapse=', ')
        )
    })
    
    output$Plot_word <- renderPlotly({
        pred<-predict_word(data(),top())
        g<-ggplot(pred, aes(x=word,freq*100)) + 
            geom_bar(stat="identity", aes(fill=Ngram))+
            facet_wrap("Ngram",ncol=1,scales = "free") + coord_flip() +
            labs(title = "Top Next Words", x="", y = "Freq (%)")
        ggplotly(g)
    })    
    output$table <- renderDataTable({
        predict_word(data(),top())
    })    
    
    output$table_stats<- renderDataTable({
        stats
       }) 

})
