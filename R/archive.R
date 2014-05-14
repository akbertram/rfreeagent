



#' ArchiveReceipts
#' 
#' Creates an archive of all attachments in 
#' FreeAgent
#' 
#' @export
ArchiveReceipts <- function(outputDir) {
  
  for(i in 1:nrow(bills)) {
    
  }
  
}

ArchiveBills <- function(outputDir) {
  
  bills <- GetFreeAgentBankBills()
  contacts <- GetFreeAgentContacts()
  contacts <- contacts[, c("url", "organisation.name", "first.name", "last.name") ]
                        
  bills <- merge(bills, contacts, by.x="contact", by.y="url", all.x=TRUE)
  vendor <- ifelse(is.na(bills$organisation.name), paste(bills$last.name, bills$first.name), bills$organisation.name)
  description <- paste(vendor, bills$reference)
  
  ArchiveFiles(file.path(outputDir, "bills"), vendor, description, bills)
}

ArchiveTransactions <- function(outputDir) {
  
  bankAccounts <- GetFreeAgentBankAccounts()
  for(i in 1:nrow(bankAccounts)) {
    ArchiveBankTransactions(bankAccounts$url[i], file.path(outputDir, bankAccounts$name[i]))
  }
}

ArchiveBankTransactions <- function(accountUrl, outputDir) {

  ex <- GetFreeAgentBankExplanations(accountUrl)
  year <- strftime(ex$dated.on, "%Y")
  description <- ex$description
  
  ArchiveFiles(outputDir, year, description, ex)
}

ArchiveFiles <- function(outputDir, subDirName, description, ex) {

  hasAttachment <- !is.na(ex$attachment.url)
    
  # Create directory structure
  dir.create(outputDir, recursive=TRUE, showWarnings=FALSE)
  for(subDir in unique(subDirName[hasAttachment])) {
    dir.create(file.path(outputDir, subDir), recursive=TRUE)
  }
  
  for(i in which(hasAttachment)) {
    fileName <- paste(strftime(ex$dated.on[i], "%Y-%m-%d"), description[i], ex$attachment.file.name[i], sep="_")
    fileName <- gsub(x=fileName, "[\\s\\(\\)#_/\\\\]+", "_", perl=TRUE)
    fileName <- gsub(x=fileName, "[_-]+", "_", perl=TRUE)
    
    filePath <- file.path(
      outputDir, 
      subDirName[i],
      fileName)
    
    # get a fresh url for amazon just in case
    attachment <- GetFreeAgentResource(ex$attachment.url[i])$attachment
    
    fetchFile(attachment$content_src, filePath)
  }
  
}

fetchFile <- function(url, file) {
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=TRUE, 
        .opts = list(
            httpheader = c(  'User-Agent' = 'rfreeagent' )))
  close(f)
  return(a)
}

ArchiveExpenses <- function(outputDir) {
  
  ex <- GetFreeAgentExpenses()
  users <- GetFreeAgentUsers()
  
  ex <- merge(ex, users, by.x="user", by.y="url", all.x=TRUE)
  name <- file.path(strftime(ex$dated.on, "%Y"), paste(m$last.name, m$first.name))
  
  ArchiveFiles(file.path(outputDir, "expenses"), name, ex$description, ex)
  
}

