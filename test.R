setwd('c:/Rdata')

df = read.csv("beverage.csv")

head(df)

summary(df)

# �ܼ�ȸ�� : ���� ������ �Ǹŷ� ���� �������
model = lm(PRICE ~ QTY, data = df)
summary(model)
# ����ȸ�� : ��� ������ �����˻� ���� ������ ���� ���� �������� ���Ե� ����

model <- lm(salary ~ experience + score, data = df)
summary(model)